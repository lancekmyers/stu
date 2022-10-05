{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module CodeGen  where

import AST
import Analysis
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Builder (Builder)
import qualified Text.Builder as Builder
import Types
import Control.Comonad.Trans.Cofree (CofreeF(..), tailF)
import Data.Functor.Foldable

import Data.String (IsString (..))
import Control.Monad.Free (_Free)
import Control.Monad.Reader (Reader, MonadReader (ask, local), runReader)
import Data.Maybe (mapMaybe)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))

{-
Preamble should look like 

from tensorflow_probability.substrates import jax as tfp
import tfp.
-}

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


-- necessary libraries
-- preamble :: Builder 
-- preamble = $(embedStringFile "app/preamble.txt")

data PyCode 
  = PyBlock [PyCode] -- maybe use DList
  | PyAssign Text PyExp  
  | PyDestr  [Text] PyExp 
  | PyDef (Maybe Text) Text [Text] PyCode
  | PyFor Text PyExp PyCode
  | PyDo PyExp 
  | PyRet PyExp 
  | PyImport Text
  deriving Show
data PyExp 
  = PyApply  PyExp [PyExp] [(Text, PyExp)]
  | PyMethod PyExp Text [PyExp]
  | PyIdent [Text] Text -- python identifier
  | PyList [PyExp]
  | PyDict [(Text, PyExp)]
  | PyNum (Either Double Int)
  | PyGet PyExp PyExp
  | PyDot PyExp Text
  | PyStr Text
  deriving Show 

instance IsString PyExp where
  fromString = PyIdent [] . T.pack 

prettyExp :: PyExp -> Builder
prettyExp (PyIdent [] ident) = Builder.text ident 
prettyExp (PyIdent modules ident) = 
  (Builder.intercalate "." $ Builder.text <$> modules) <> "." <> Builder.text ident
prettyExp (PyList xs) = "[" <> Builder.intercalate "," (prettyExp <$> xs) <> "]"
prettyExp (PyDict xs) = "{" <> Builder.intercalate "," 
  ["'" <> Builder.text k <> "'" <> " : " <> prettyExp v | (k, v) <- xs] <> "}"
prettyExp (PyNum (Right i)) = Builder.decimal i
prettyExp (PyNum (Left f)) = Builder.string $ show f
prettyExp (PyGet x i) = prettyExp x <> "[" <> prettyExp i <> "]"
prettyExp (PyDot x i) = prettyExp x <> "." <> Builder.text i
prettyExp (PyStr txt) = "'" <> Builder.text txt <> "'"
prettyExp (PyApply f xs []) = fname <> "(" <> args <> ")"
  where 
    fname = prettyExp f
    args   = Builder.intercalate "," (prettyExp <$> xs) 
prettyExp (PyApply f [] kws) = fname <> "(" <> kwArgs <> ")"
  where 
    fname = prettyExp f
    kwArgs = Builder.intercalate "," 
      [Builder.text kw <> "=" <> prettyExp v | (kw,v) <- kws]
prettyExp (PyApply f xs kws) = fname <> "(" <> args <> "," <> kwArgs <> ")"
  where 
    fname = prettyExp f
    args   = Builder.intercalate "," (prettyExp <$> xs) 
    kwArgs = Builder.intercalate "," 
      [Builder.text kw <> "=" <> prettyExp v | (kw,v) <- kws]
prettyExp (PyMethod x f xs) = (prettyExp x) <> "." <> 
  (Builder.text f) <> "(" <> Builder.intercalate "," (prettyExp <$> xs) <> ")"


indent :: Builder -> Reader Int Builder
indent b = do 
  n <- ask 
  pure $ Builder.text (T.replicate n "  ") <> b 

prettyCode :: PyCode -> Reader Int Builder 
prettyCode (PyBlock stmts) = Builder.intercalate "\n" <$> 
  traverse prettyCode stmts 
prettyCode (PyDo exp) = indent $ prettyExp exp
prettyCode (PyAssign name val) = indent $ 
  Builder.text name <> " = " <> prettyExp val 
prettyCode (PyDestr names val) = indent $ 
  (Builder.intercalate ", " . fmap Builder.text $ names) <> " = " <> prettyExp val 
prettyCode (PyRet x) = indent $ "return (" <> prettyExp x <> ")"
prettyCode (PyImport x) = indent $ "import " <> Builder.text x
prettyCode (PyFor x iter body) = do 
  line <- indent $ "for " <> Builder.text x <> " in " <> prettyExp iter <> ":" 
  body <- local (+1) $ prettyCode body
  return $ line <> "\n" <> body
prettyCode (PyDef Nothing name args body) = do  
  decl <- indent $ "def " <> Builder.text name <> 
    "(" <> (Builder.intercalate ", " . fmap Builder.text $ args) <> "):"
  body <- local (+1) $ prettyCode body
  return $ decl <> "\n" <> body
prettyCode (PyDef (Just dec) name args body) = do 
  decorator <- indent $ "@" <> Builder.text dec <> "\n"
  decl <- indent $ "def " <> Builder.text name <> 
    "(" <> (Builder.intercalate ", " . fmap Builder.text $ args) <> "):"
  body <- local (+1) $ prettyCode body
  return $ decorator <> decl <> "\n" <> body

jnp :: Text -> PyExp
jnp name = PyIdent ["jnp"] name
tfd :: Text -> PyExp
tfd name = PyIdent ["tfd"] name
tfb :: Text -> PyExp
tfb name = PyIdent ["tfb"] name
(@@) :: PyExp -> [PyExp] -> PyExp
f @@ x = PyApply f x []

(!$) :: PyExp -> Text -> [PyExp] -> PyExp 
x !$ f = PyMethod x f  
(!.) :: PyExp ->Text -> PyExp 
x !. i = PyDot x i
(!) :: PyExp -> PyExp -> PyExp 
x ! i = PyGet x i


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
    go (VarF name Val)   = PyIdent [] (name <> "val")
    go (VarF name Data)  = PyGet (PyIdent [] "obs") (PyStr name)
    go (VarF name Param) = PyIdent [] (name <> "_tr")
    go (FunAppF f xs) = jnp f @@ xs
    go (GatherF xs is) = jnp "gather" @@ [xs, is]


sample :: Maybe Shape -> PyExp -> PyExp 
sample Nothing dist = dist 
sample (Just sh) dist = sampled 
  where
    cgCard (CardFV n) = "cards" ! PyStr n
    cgCard (CardN n) = PyNum $ Right n
    cgShape = PyList . V.toList . fmap cgCard 
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
    obs = PyGet (PyIdent [] "obs") (PyStr name)
    lp = PyAssign (name <> "_lp") $ 
      PyMethod (cgDistribution dist) "log_prob" [obs]
cgModelStmt (ValStmt name _ x) = PyAssign (name <> "_val") $ cgExpr x

cgModel :: Program a -> PyCode 
cgModel (Program decls model@(Model stmts)) = 
    PyBlock [cgBijDict model, mk_log_prob]
  where 
    mk_log_prob = PyDef Nothing "mk_log_prob" ["obs"] 
      (PyBlock [cgCardAssign decls,  log_prob, PyRet $ PyIdent [] "log_prob"])

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


cgAbsShapes :: [Decl] -> PyCode 
cgAbsShapes decls = PyAssign "abs_shapes" $ PyDict 
  [(name, cgTy ty) |(name, ty) <- dats]
  where
    go (DataDecl name ty) = Just (name, ty)
    go _ = Nothing
    dats = mapMaybe go decls
    cgCard (CardN  n) = PyNum $ Right n
    cgCard (CardFV x) = PyStr x 
    cgTy (Ty sh el) = PyList (V.toList . fmap cgCard $ sh)

cgCardAssign :: [Decl] -> PyCode
cgCardAssign decls = PyBlock [ cgAbsShapes decls, 
    PyAssign "cards" (PyDict []), 
    forLoop ]
  where
    forLoop = PyFor "blah" 
      ("zip" @@ ["obs" !$ "keys" $ [], "obs" !$ "values" $ []]) body 
    body = PyBlock [
        PyDestr ["name", "x"] "blah",
        PyAssign "abs_shape" ("abs_shapes" ! "name"),
        PyFor "nm" ("zip" @@ ["abs_shape", "x" !. "shape"]) $ PyBlock [
          PyAssign "n" ("nm" ! (PyNum $ Right 0)),
          PyAssign "m" ("nm" ! (PyNum $ Right 1)),
          PyDo $ "cards" !$ "update" $ [
            PyList [ PyList [ "n", "m" ] ]
            ]
          ]
      ]

cgPriorPredGo :: (Text, Distribution a) -> Int -> PyCode 
cgPriorPredGo (name, dist) i = 
  PyAssign (name <> "_samples") $ (cgDistribution dist !$ "sample" $ ["sample_shape", "keys" ! (PyNum $ Right i)])

cgPriorPred :: Program a ->  PyCode 
cgPriorPred (Program _ (Model xs)) = PyDef Nothing "prior_pred" ["key", "sample_shape"] $ PyBlock 
    [ PyAssign "keys" $ 
        PyIdent ["jax", "random"] "split" @@ ["key", PyNum . Right $ length xs ]
    , params_sample, observ_sample
    , PyAssign "prior_samples" $ prior_dict
    , PyAssign "prior_predictive_samples" $ prior_pred_dict
    , PyRet $ PyApply (PyIdent ["az"] "from_dict") [] 
        [ ("prior", "prior_samples")
        , ("prior_predictive", "prior_predictive_samples")]
    ]
  where 
    params = mapMaybe (\case (ParamStmt x _ d _) -> Just (x, d); _ -> Nothing) xs 
    observ = mapMaybe (\case (ObsStmt x d) -> Just (x, d); _ -> Nothing) xs 

    params_sample = PyBlock $ zipWith cgPriorPredGo params [0..]
    observ_sample = PyBlock $ zipWith cgPriorPredGo observ [0..]

    prior_dict = PyDict [ 
      (name, PyIdent [] $ "name" <> "_samples") | (name,_) <- params ]
    prior_pred_dict = PyDict [ 
      (name, PyIdent [] $ "name" <> "_samples") | (name,_) <- observ ]


  

cgProg :: Program a -> PyCode
cgProg prog = PyBlock [preamble, cgModel prog, cgPriorPred prog]


writeProg :: Program a -> Builder
writeProg prog = runReader (prettyCode $ cgProg prog) 0
