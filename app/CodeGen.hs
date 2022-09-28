{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module CodeGen  where

import AST
import Analysis
import Data.Text (Text)
import qualified Data.Text as T
import Text.Builder (Builder)
import qualified Text.Builder as Builder
import Types
import Control.Comonad.Trans.Cofree (CofreeF(..))
import Data.Functor.Foldable

import Data.String (IsString (..))
import Control.Monad.Free (_Free)
import Control.Monad.Reader (Reader, MonadReader (ask, local), runReader)
import Data.Maybe (mapMaybe)

{-
Preamble should look like 

import arviz as az
import jax.numpy as jnp
import distrax
import blackjax 

-}

cgBijDict :: forall a. Model a -> PyCode 
cgBijDict (Model stmts) 
  = PyAssign "bijectors" bijectors
  where 
    go (ParamStmt name _ _ (Just bij)) = Just (name, cgBij bij)
    go (ParamStmt name _ _ Nothing) = Just (name, distrax "Identity" @@ [])
    go _ = Nothing
    bijectors :: PyExp
    bijectors = PyDict . mapMaybe go $ stmts


cgBij :: Bijector a -> PyExp  
cgBij = fold (\(_ :< x) -> alg x) where 
  alg :: BijectorF PyExp -> PyExp
  alg (MkBij name args) = (distrax name) @@ (PyNum . Left <$> args)
    
  alg (Chain bs) = distrax "chain" @@ [PyList bs]


-- necessary libraries
-- preamble :: Builder 
-- preamble = $(embedStringFile "app/preamble.txt")

data PyCode 
  = PyBlock [PyCode] -- maybe use DList
  | PyAssign Text PyExp  
  | PyDestr  [Text] PyExp 
  | PyDef (Maybe Text) Text [Text] PyCode
  | PyRet PyExp 
  deriving Show
data PyExp 
  = PyApply  PyExp [PyExp]
  | PyMethod PyExp Text [PyExp]
  | PyIdent [Text] Text -- python identifier
  | PyList [PyExp]
  | PyDict [(Text, PyExp)]
  | PyNum (Either Double Int)
  | PyGet PyExp PyExp
  | PyStr Text
  deriving Show 

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
prettyExp (PyStr txt) = "'" <> Builder.text txt <> "'"
prettyExp (PyApply f xs) = (prettyExp f) <> "(" <> Builder.intercalate "," (prettyExp <$> xs) <> ")"
prettyExp (PyMethod x f xs) = (prettyExp x) <> "." <> 
  (Builder.text f) <> "(" <> Builder.intercalate "," (prettyExp <$> xs) <> ")"


indent :: Builder -> Reader Int Builder
indent b = do 
  n <- ask 
  pure $ Builder.text (T.replicate n "  ") <> b 

prettyCode :: PyCode -> Reader Int Builder 
prettyCode (PyBlock stmts) = Builder.intercalate "\n" <$> 
  traverse prettyCode stmts 
prettyCode (PyAssign name val) = indent $ 
  Builder.text name <> " = " <> prettyExp val 
prettyCode (PyDestr names val) = indent $ 
  (Builder.intercalate ", " . fmap Builder.text $ names) <> " = " <> prettyExp val 
prettyCode (PyRet x) = indent $ "return (" <> prettyExp x <> ")"
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
distrax :: Text -> PyExp
distrax name = PyIdent ["distrax"] name
(@@) :: PyExp -> [PyExp] -> PyExp
f @@ x = PyApply f x

cgExpr :: Expr w -> PyExp
cgExpr = cata (go . proj)
  where 
    proj (_ :< expr) = expr
    go :: ExprF PyExp -> PyExp
    go (ArithF op x y) = case op of
      Add -> jnp "add" @@ [x, y]
      Mul -> jnp "mul" @@ [x, y]
      Sub -> jnp "sub" @@ [x, y]
      Div -> jnp "div" @@ [x, y]
    go (LitInt n) = PyNum (Right n)
    go (LitReal x) = PyNum (Left x) 
    go (LitArray xs) = PyList xs
    go (VarF name) = PyIdent [] (name <> "_tr")
    go (FunAppF f xs) = jnp f @@ xs
    go (GatherF xs is) = jnp "gather" @@ [xs, is]


cgDistribution :: Distribution a -> PyExp
cgDistribution (Distribution name args) = distrax name @@ (cgExpr <$> args)

ld_tr :: Text -> PyCode 
ld_tr name = PyDestr [name_ld, name_tr] $ 
  PyMethod bij "forward_and_log_prob" [PyGet (PyIdent [] "params") (PyStr name)]
  where 
    bij = PyGet (PyIdent [] "bijectors") (PyStr name)
    name_ld = name <> "_ld"
    name_tr = name <> "_tr"

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
cgModelStmt (ValStmt name _ x) = PyAssign (name <> "_tr") $ cgExpr x

cgModel :: Model a -> PyCode 
cgModel model@(Model stmts) = PyBlock [cgBijDict model, mk_log_prob]
  where 
    mk_log_prob = PyDef Nothing "mk_log_prob" ["obs"] 
      (PyBlock [log_prob, PyRet $ PyIdent [] "log_prob"])

    log_prob = PyDef (Just "jax.jit") "log_prob" ["params"] $ 
      PyBlock [PyBlock $ cgModelStmt <$> stmts, PyRet $ lpSum stmts]

    lpSum :: [ModelStmt a] -> PyExp 
    lpSum ((ParamStmt name _ _ _):xs) = jnp "add" @@ 
      [PyIdent [] (name <> "_lp"), lpSum xs] 
    lpSum ((ObsStmt name _):xs) = jnp "add" @@ 
      [PyIdent [] (name <> "_lp"), lpSum xs] 
    lpSum ((ValStmt _ _ _):xs) = lpSum xs
    lpSum [] = PyNum (Left 0)

    



writeModel :: Model a ->  Builder
writeModel model = runReader (prettyCode $ cgModel model) 0

writeProg :: Program a -> Builder
writeProg (Program _  model) = writeModel model
{-
cgProg (decls, model) = do
  modelCode <- cgModel model
  return $
    Builder.intercalate
      "\n\n"
      [preamble, cgConstraints decls, modelCode]
-}

{-
cgModelStmt :: ModelStmt a -> Builder 
cgModelStmt (ParamStmt name ty dist _) = _
  where 
    nb = Builder.text name
    _ = " = bijectors['" <> nb <> "'].log_and_det_forward(params['" <> nb <> "'])"
-}

{-
cgModelStmt :: (MonadTyCtx m) => ModelStmt -> m Builder
cgModelStmt (ParamStmt name ty dist) = do
  dist_code <- cgDistribution name dist
  return $
    mconcat
      [ "\t#",
        Builder.text name,
        " : ",
        Builder.string $ show ty,
        "\n",
        "\t",
        Builder.text name,
        " = yield ",
        dist_code
      ]
cgModelStmt (ValStmt name ty val) =
  return $
    mconcat
      [ "\t#",
        Builder.text name,
        " : ",
        Builder.string $ show ty,
        "\n",
        "\t",
        Builder.text name,
        " = ",
        cgExpr val
      ]
cgModelStmt (ObsStmt name dist) = do
  dist_code <- cgDistribution name dist
  return $
    mconcat
      [ "\t",
        "yield ",
        dist_code
      ]

cgModelJD :: (MonadTyCtx m) => Model -> m Builder
cgModelJD (Model stmts) = do
  model_stmts <- sequenceA $ cgModelStmt <$> stmts
  return $
    Builder.intercalate
      "\n"
      [ "@tfd.JointDistributionCoroutineAutoBatched",
        "def joint_dist():",
        Builder.intercalate "\n" $ model_stmts
      ]

-- generate constraints

cgConstraints :: [Decl] -> Builder
cgConstraints decls =
  "tf.debuging.assert_shapes([" <> Builder.intercalate ", " constraints <> "])"
  where
    isData (DataDecl _ _) = True
    isData _ = False

    dataDecls = filter isData decls
    constraints :: [Builder]
    constraints =
      [ "(" <> Builder.text name <> ", " <> cgShape sh <> ")"
        | DataDecl name (Ty sh _) <- dataDecls
      ]

cgCard :: Card -> Builder
cgCard (CardN n) = Builder.decimal n
cgCard (CardFV v) = "\"" <> Builder.text v <> "\""
cgCard (CardBV i) = "\"" <> Builder.string (show i) <> "\""

cgShape :: Shape -> Builder
cgShape sh = "(" <> (Builder.intercalate ", " (cgCard <$> sh)) <> ")"

cgPinnedLogProb :: Builder 
cgPinnedLogProb = Builder.intercalate "\n" 
  [ "pinned_joint_dist = joint_dist.experimental_pin(obs.to_dict()['observed_data'])"
  , "@tf.function(autograph=False, jit_compile=False)"
  , "def target_log_prob(*x):"
  , "\t return pinned_joint_dist.unnormalized_log_prob(x)"
  ]

cgProg (decls, model) = do
  modelCode <- cgModelJD model
  return $
    Builder.intercalate
      "\n\n"
      [preamble, cgConstraints decls, modelCode]
-}
