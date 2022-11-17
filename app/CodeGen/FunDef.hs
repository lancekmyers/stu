module CodeGen.FunDef where

import AST
import Data.Text (Text)
import qualified Data.Vector as V
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
import CodeGen.Expr
import qualified Data.Text as T

cgFunDef :: FunDef a -> PyCode
cgFunDef (FunDef name args ret body) = PyDef Nothing name' argNames body' 
    where 
        argNames = ("_bd_" <>) . fst <$> args 
        name' = "_func_" <> name
        body' = cgFunBody body

cgFunBody :: FunBody a -> PyCode 
cgFunBody (FunRet val) = PyRet $ cgExpr val
cgFunBody (LetPrimIn x _ pApp rest) = 
    PyBlock [ PyAssign ("_local_" <> x) (cgPrimApp pApp),  cgFunBody rest ] 
cgFunBody (FunLetIn x _ val rest) = 
    PyBlock [ PyAssign ("_local_" <> x) (cgExpr val),  cgFunBody rest ]


cgPrimApp :: PrimApp a -> PyExp
cgPrimApp (PrimApp name args) = (PyIdent mod f) @@ (cgExpr <$> args)
    where 
        mod = init name 
        f = last name
