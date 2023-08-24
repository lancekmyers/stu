{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AST where

import Control.Comonad.Trans.Cofree (Cofree)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Megaparsec.Pos (SourcePos)
import Types (Shape, Ty)
import Util (SrcSpan)

type Name = Text

type FuncName = Text

-- Phases of compilation
data Parsing

data Elaboration

-- | Annotation associated with phase
type family PhAnn a where 
  PhAnn Parsing = SrcSpan
  PhAnn Elaboration = Ty

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

-- data VarDomain
--   = Unknown
--   | Param
--   | Data
--   | Val
--   | Bound
--   | Local
--   deriving (Eq, Ord, Show)

data ExprF ph a where 
  ArithF 
    :: BinOp -> a -> a -> ExprF ph a
  VarF 
    :: Name -> ExprF ph a
  -- would like to annotate with extra info during 
  FunAppF 
    :: FuncName -> [a] -> ExprF ph a
  TransposeF 
    :: a -> [Int] -> ExprF ph a
  FoldF 
    :: FuncName -> a -> a -> ExprF ph a
  -- mul e xs
  ScanF 
    :: FuncName -> a -> a -> ExprF ph a
  BroadcastF
    :: Shape -> Shape -> a -> ExprF ph a
  LitReal 
    :: Double -> ExprF ph a
  LitInt 
    :: Int -> ExprF ph a
  LitArray 
    :: [a] -> ExprF ph a

deriving instance Functor (ExprF ph)
deriving instance Foldable (ExprF ph)
deriving instance Traversable (ExprF ph)

type Expr ph = Cofree (ExprF ph) (PhAnn ph)

type ExprSrc = Expr Parsing

type DistName = Text

data Distribution ph
  = Distribution
      DistName
      [Expr ph]
      (PhAnn ph)
      (Maybe Int, Maybe Shape) --- nbatch_dims, sample_shape
  deriving ()

data Bijector 
  = MkBij Text [Double]
  | Chain [Bijector]
  deriving (Show, Eq, Ord)


data Decl
  = CardDecl Name
  | FactorDecl Name
  | DataDecl Name Ty
  deriving (Show)

-- include shape that is being broadcast over?

-- | Statements involved in the model
data ModelStmt ph
  = ValStmt Name Ty (Expr ph)
  | ParamStmt Name Ty (Distribution ph) (Maybe Bijector)
  | ObsStmt Name (Distribution ph)

data Model ph
  = Model [ModelStmt ph]

data Program ph = Program
  { decls :: [Decl],
    model :: Model ph
  }

---

data BijDef ph = BijDef (FunDef ph) (FunDef ph) (FunDef ph)

-- fwd inv ldj

data FunDef ph = FunDef
  { _funName :: Text,
    _args :: [(Text, Ty)],
    _ret :: Ty,
    _body :: (FunBody ph)
  }

instance Show (FunDef a) where
  show (FunDef name args _ _) = show name ++ ' ' : show args

data DistDef ph = DistDef
  { _distName :: Text,
    _params :: [(Text, Ty)],
    _eventTy :: Ty,
    _lpdf :: FunDef ph,
    _sample :: SampleBody ph,
    _bij :: Bijector
  }

data FunBody ph
  = LetPrimIn Text Ty (PrimApp ph) (FunBody ph)
  | FunLetIn Text Ty (Expr ph) (FunBody ph)
  | FunRet (Expr ph)

data SampleBody ph
  = SampleIn Text Ty (Distribution ph) (SampleBody ph)
  | SampleUnifIn Text Ty (SampleBody ph)
  | SampleLetIn Text Ty (Expr ph) (SampleBody ph)
  | SampleRet (Expr ph)

data Library ph = Library
  { _funs :: [FunDef ph],
    _dists :: [DistDef ph]
  }

data PrimApp ph = PrimApp [Text] [Expr ph]

data PrimSample = PrimUniformRNG | PrimNormalRNG
