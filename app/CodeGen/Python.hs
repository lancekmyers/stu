module CodeGen.Python where 

import Data.Text (Text)
import qualified Data.Text as T
import Text.Builder (Builder)
import qualified Text.Builder as Builder
import Data.String (IsString (..))
import Control.Monad.Reader (Reader, MonadReader (ask, local), runReader)

data PyCode 
  = PyBlock [PyCode] -- maybe use DList since these get appended a fair bit
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
