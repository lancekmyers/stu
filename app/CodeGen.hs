{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module CodeGen  where

import AST
import Data.Text (Text)
import qualified Data.Vector as V
import Text.Builder (Builder)
import Types
import Control.Comonad.Trans.Cofree (CofreeF(..), tailF)
import Data.Functor.Foldable ( fold, Recursive(cata) )
import Data.String (IsString (..))
import Control.Monad.Reader ( runReader, MonadReader, asks ) 
import Data.Maybe (mapMaybe, catMaybes)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))
import qualified Data.Map as M

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
import Analysis (Ctx)
import Analysis.Context (Ctx(dists), lookupDistDefaultBij)
import qualified Data.Text as T

type CodeGenMonad m = (MonadReader Ctx m)

preamble :: PyCode
preamble = PyBlock [
  PyImport "tensorflow_probability.substrates.jax as tfp",
  PyImport "tensorflow_probability.substrates.jax.distributions as tfd", 
  PyImport "tensorflow_probability.substrates.jax.bijectors as tfb", 
  PyImport "jax", 
  PyImport "jax.numpy as jnp",
  PyImport "arviz as az"
  ]


cgBijDict :: forall a m. CodeGenMonad m => Model a -> m PyCode 
cgBijDict (Model stmts) = do 
  kv <- traverse go stmts  
  let bijectors = PyDict (catMaybes kv)
  return $ PyAssign "bijectors" bijectors
  where 
    go :: ModelStmt a -> m (Maybe (Text, PyExp))
    go (ParamStmt name _ _ (Just bij)) = return $ Just (name, cgBij bij)
    go (ParamStmt name _ (Distribution dname _ _ _) Nothing) = do 
      distCtx <- asks dists
      return $ case M.lookup dname distCtx of 
        Just (_fty, bij) -> Just (name, cgBij bij)
        Nothing -> error $ "This is a bug. Unknown dist " ++ T.unpack dname
    go _ = return Nothing


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

cgFun (FunAppF "mean" xs) = PyApply "jnp.mean" xs [("axis", PyNum $ Right (-1))]
cgFun (FunAppF f xs) = case f of 
  "matmul" -> jnp "dot" @@ xs
  "matvec" -> jnp "dot" @@ xs
  "vecmat" -> jnp "dot" @@ xs
  "dot" -> jnp "dot" @@ xs

  "sqrt" -> jnp "sqrt" @@ xs
  "exp" -> jnp "exp" @@ xs
  "log" -> jnp "log" @@ xs

  "sin" -> jnp "sin" @@ xs
  "cos" -> jnp "cos" @@ xs
  "tan" -> jnp "tan" @@ xs

  "tanh" -> jnp "tanh" @@ xs
  "sinh" -> jnp "sinh" @@ xs
  "cosh" -> jnp "cosh" @@ xs

  "asin" -> jnp "asin" @@ xs
  "acos" -> jnp "acos" @@ xs
  "atan" -> jnp "atan" @@ xs

  _ -> error $ "Encoutered function " ++ (T.unpack f) ++ " during codegen"


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

cgModel :: forall a m. CodeGenMonad m => Program a -> m PyCode 
cgModel (Program decls model@(Model stmts)) = do 
  bijDict <- cgBijDict model  
  return $ PyBlock [bijDict, mk_log_prob]
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


  

cgProg :: forall a m. CodeGenMonad m => Program a -> m PyCode
cgProg prog = do 
  model' <- cgModel prog
  return $ PyBlock [preamble, model', cgPriorPred prog]


writeProg :: forall a m. CodeGenMonad m => Program a -> m Builder
writeProg prog = do 
  prog' <- cgProg prog
  return $ runReader (prettyCode prog') 0
