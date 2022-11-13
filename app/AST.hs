{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, StandaloneDeriving #-}

module AST where

import Control.Comonad.Trans.Cofree (CofreeF(..), Cofree)

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics ( Generic, (:+:) )
import Types (Ty, Shape)
import Data.Fix 
import Text.Megaparsec.Pos (SourcePos)

type Name = Text

type FuncName = Text


data BinOp
  = Add
  | Sub
  | Mul
  | Div

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data VarDomain = Unknown | Param | Data | Val deriving (Eq, Ord, Show)

data ExprF a
  = ArithF BinOp a a
  | VarF Name VarDomain
  | FunAppF FuncName [a]
  | GatherF a a
  | LitReal Double
  | LitInt Int
  | LitArray [a]
  deriving (Generic, Functor, Foldable, Traversable)


type Expr ann = Cofree ExprF ann 
type ExprSrc = Expr SourcePos


type DistName = Text

data Distribution ann 
  = Distribution 
      DistName 
      [Expr ann] 
      ann 
      (Maybe Int, Maybe Shape) --- nbatch_dims, sample_shape
  deriving ()

data BijectorF a  
  = MkBij Text [Double]
  | Chain [a]
  deriving (Generic, Functor, Foldable, Traversable)

type Bijector ann = Cofree BijectorF ann


data Decl
  = CardDecl Name
  | FactorDecl Name
  | DataDecl Name Ty
  deriving (Show)

-- include shape that is being broadcast over?
-- | Statements involved in the model 
data ModelStmt ann 
  = ValStmt   Name Ty (Expr ann) 
  | ParamStmt Name Ty (Distribution ann) (Maybe (Bijector ann)) 
  | ObsStmt   Name (Distribution ann)


data Model ann
  = Model [ModelStmt ann]

data Program ann = Program 
  { decls :: [Decl]
  , model :: Model ann
  }
--- 

data BijDef a = BijDef (FunDef a) (FunDef a) (FunDef a) 
  -- fwd inv ldj 

data FunDef ann = FunDef 
  { _funName :: Text
  , _args :: [(Text, Ty)]
  , _ret  :: Ty 
  , _body :: (FunBody ann)
  }

data DistDef ann = DistDef 
  { _distName :: Text 
  , _params :: [(Text, Ty)] 
  , _eventTy :: Ty
  , _lpdf :: FunDef ann
  , _sample :: SampleBody ann
  , _bij :: Bijector ann
  }

data FunBody ann 
  = LetPrimIn Text Ty (PrimApp ann) (FunBody ann)
  | FunLetIn  Text Ty (Expr ann) (FunBody ann) 
  | FunRet    (Expr ann)

data SampleBody ann 
  = SampleIn    Text Ty (Distribution ann) (SampleBody ann)
  | SampleLetIn Text Ty (Expr ann) (SampleBody ann) 
  | SampleRet    (Expr ann)

data Library a = Library {
  _funs  :: [FunDef  a],
  _dists :: [DistDef a] 
}

data PrimApp  a = PrimApp Text [Expr a]
data PrimSample = PrimUniformRNG | PrimNormalRNG 
