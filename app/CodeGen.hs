{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module CodeGen  where

import AST
import Data.Text (Text)
import qualified Data.Vector as V
import Text.Builder (Builder)
import Types
import Control.Comonad.Trans.Cofree (CofreeF(..), tailF)
import Data.Functor.Foldable ( fold, Recursive(cata) )
import Data.String (IsString (..))
import Control.Monad.Reader ( runReader ) 
import Data.Maybe (mapMaybe)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))

import CodeGen.Python
    ( PyExp(..),
      PyCode(..),
      prettyCode,
      jnp,
      tfd,
      tfb,
      (@@),
      (!$),
      (!.),
      (!) ) 


preamble :: PyCode
preamble = PyBlock [
  PyImport "tensorflow_probability.substrates.jax as tfp",
  PyImport "tensorflow_probability.substrates.jax.distributions as tfd", 
  PyImport "tensorflow_probability.substrates.jax.bijectors as tfb", 
  PyImport "jax", 
  PyImport "jax.numpy as jnp",
  PyImport "arviz as az"
  ]


cgBijDict :: forall a. Model a -> PyCode 
cgBijDict (Model stmts) 
  = PyAssign "bijectors" bijectors
  where 
    go (ParamStmt name _ _ (Just bij)) = Just (name, cgBij bij)
    go (ParamStmt name _ _ Nothing) = Just (name, tfb "Identity" @@ [])
    go _ = Nothing
    bijectors :: PyExp
    bijectors = PyDict . mapMaybe go $ stmts


cgBij :: Bijector a -> PyExp  
cgBij = fold (alg . tailF . runIdentity . getCompose) where 
  alg :: BijectorF PyExp -> PyExp
  alg (MkBij name args) = (tfb name) @@ (PyNum . Left <$> args)
    
  alg (Chain bs) = tfb "Chain" @@ [PyList bs]


cgExpr :: Expr w -> PyExp
cgExpr = cata (go . proj)
  where 
    proj = tailF . runIdentity . getCompose
    go :: ExprF PyExp -> PyExp
    go (ArithF op x y) = case op of
      Add -> jnp "add" @@ [x, y]
      Mul -> jnp "multiply" @@ [x, y]
      Sub -> jnp "subtract" @@ [x, y]
      Div -> jnp "divide" @@ [x, y]
    go (LitInt n) = PyNum (Right n)
    go (LitReal x) = PyNum (Left x) 
    go (LitArray xs) = PyList xs
    go (VarF name Unknown) = PyIdent [] (name <> "_ack")
    go (VarF name Val)   = PyIdent [] (name <> "_val")
    go (VarF name Data)  = PyGet (PyIdent [] "data") (PyStr name)
    go (VarF name Param) = PyIdent [] (name <> "_tr")
    go (FunAppF "mean" xs) = PyApply "jnp.mean" xs 
      [("axis", PyNum $ Right (-1))]
    go (FunAppF f xs) = jnp f @@ xs
    go (GatherF xs is) = PyGet xs is -- jnp "gather" @@ [xs, is]


sample :: Maybe Shape -> PyExp -> PyExp 
sample Nothing dist = dist 
sample (Just sh) dist = sampled 
  where
    cgCard (CardFV n) = "cards" ! PyStr n
    cgCard (CardN n) = PyNum $ Right n
    cgShape = PyList . V.toList . fmap cgCard . getVec
    sampled = PyApply (tfd "Sample") [dist] 
      [("sample_shape", cgShape sh)]

independent :: Maybe Int -> PyExp -> PyExp 
independent Nothing dist = dist 
independent (Just n) dist = batched
  where 
    batched = PyApply (tfd "Independent") [dist] 
      [("reinterpreted_batch_ndims", PyNum $ Right n)]

cgDistribution :: Distribution a -> PyExp
cgDistribution (Distribution name args _ (bd, br_sh)) = 
  sample br_sh . independent bd $ tfd name @@ (cgExpr <$> args)

ld_tr :: Text -> PyCode 
ld_tr name = PyBlock [ld, tr]
  where 
    param = [PyGet (PyIdent [] "params") (PyStr name)]
    bij = PyGet (PyIdent [] "bijectors") (PyStr name)
    ld = PyAssign (name<>"_tr") (PyMethod bij "forward" param)
    tr = PyAssign (name<>"_ld") (PyMethod bij "forward_log_det_jacobian" param)

cgModelStmt :: ModelStmt a -> PyCode 
cgModelStmt (ParamStmt name ty dist _) = PyBlock [ld_tr name, lp] 
  where 
    param = PyIdent [] $ name <> "_tr"
    lp = PyAssign (name <> "_lp") $ 
      PyMethod (cgDistribution dist) "log_prob" [param]
cgModelStmt (ObsStmt name dist) = lp 
  where 
    obs = PyGet (PyIdent [] "data") (PyStr name)
    lp = PyAssign (name <> "_lp") $ 
      PyMethod (cgDistribution dist) "log_prob" [obs]
cgModelStmt (ValStmt name _ x) = PyAssign (name <> "_val") $ cgExpr x

cgModel :: Program a -> PyCode 
cgModel (Program decls model@(Model stmts)) = 
    PyBlock [cgBijDict model, mk_log_prob]
  where 
    mk_log_prob = PyDef Nothing "mk_log_prob" ["inf_obj"] 
      (PyBlock [cgCards, cgData, log_prob, PyRet $ PyIdent [] "log_prob"])

    log_prob = PyDef (Just "jax.jit") "log_prob" ["params"] $ 
      PyBlock [PyBlock $ cgModelStmt <$> stmts, PyRet $ lpSum stmts]

    lpSum :: [ModelStmt a] -> PyExp 
    lpSum ((ParamStmt name _ _ _):xs) = jnp "add" @@ 
      [ jnp "add" @@ 
        [ PyIdent [] (name <> "_ld")
        , PyIdent [] (name <> "_lp")]
      , lpSum xs] 
    lpSum ((ObsStmt name _):xs) = jnp "add" @@ 
      [PyIdent [] (name <> "_lp"), lpSum xs] 
    lpSum ((ValStmt _ _ _):xs) = lpSum xs
    lpSum [] = PyNum (Left 0)

cgCards :: PyCode 
cgCards = PyAssign "cards" (obser_dims !$ "__or__" $ [const_dims])
  where 
    obser_dims = "dict" @@ ["inf_obj" !. "observed_data" !. "dims"]
    const_dims = "dict" @@ ["inf_obj" !. "constant_data" !. "dims"]
cgData :: PyCode 
cgData = PyAssign "data" (obser_data !$ "__or__" $ [const_data])
  where 
    map_jnp_array x = PyIdent ["jax", "tree_util"] "tree_map" @@ 
      [PyIdent ["jnp"] "array", x]
    obser_data = map_jnp_array ("dict" @@ ["inf_obj" !. "observed_data"])
    const_data = map_jnp_array ("dict" @@ ["inf_obj" !. "constant_data"])


cgPriorPredGo :: (ModelStmt a) -> Int -> PyCode 
cgPriorPredGo (ObsStmt name dist) i = PyAssign ("data['"<>name<>"']") $ 
  (cgDistribution dist !$ 
    "sample" $ ["sample_shape", "keys" ! (PyNum $ Right i)])
cgPriorPredGo (ValStmt name _ exp) i = PyAssign (name<>"_val") $ cgExpr exp
cgPriorPredGo (ParamStmt name _ dist _) i = PyBlock 
  [ PyAssign (name <> "_tr") $
      cgDistribution dist !$ 
        "sample" $ [PyList [], "keys" ! (PyNum $ Right i)]
  , PyAssign ("params['"<>name<>"']") $ 
      cgDistribution dist !$ 
        "sample" $ ["sample_shape", "keys" ! (PyNum $ Right i)]
  ]



cgPriorPred :: Program a ->  PyCode 
cgPriorPred (Program _ (Model xs)) = PyDef Nothing "prior_pred" ["key", "sample_shape", "inf_obj"] $ PyBlock 
    [ cgCards
    , constData 
    , emptyParams
    , PyAssign "keys" $ 
        PyIdent ["jax", "random"] "split" @@ ["key", PyNum . Right $ length xs ]
    , stmts
    , PyAssign "prior_samples" $ prior_dict
    , PyAssign "prior_predictive_samples" $ prior_pred_dict
    , PyRet $ PyList [ "prior_samples", "prior_predictive_samples" ]
      {-
      PyRet $ PyApply (PyIdent ["az"] "from_dict") [] 
        [ ("prior", "prior_samples")
        , ("prior_predictive", "prior_predictive_samples")]
      -}
    ]
  where 
    
    map_jnp_array x = PyIdent ["jax", "tree_util"] "tree_map" @@ 
      [PyIdent ["jnp"] "array", x]
    constData = PyAssign "data" $ map_jnp_array 
      ("dict" @@ ["inf_obj" !. "constant_data"])
    emptyParams = PyAssign "params" (PyDict [])

    params = mapMaybe (\case (ParamStmt x _ _ _) -> Just x; _ -> Nothing) xs 
    observ = mapMaybe (\case (ObsStmt x _) -> Just x; _ -> Nothing) xs 

    stmts = PyBlock $ zipWith cgPriorPredGo xs [0..]
    -- observ_sample = PyBlock $ zipWith cgPriorPredGo observ [0..]

    prior_dict = PyDict [ 
      (name, "params" ! (PyStr name) ) | name <- params ]
    prior_pred_dict = PyDict [ 
      (name, "data" ! (PyStr name) ) | name <- observ ]


  

cgProg :: Program a -> PyCode
cgProg prog = PyBlock [preamble, cgModel prog, cgPriorPred prog]


writeProg :: Program a -> Builder
writeProg prog = runReader (prettyCode $ cgProg prog) 0
