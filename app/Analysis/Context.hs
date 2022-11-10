{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Analysis.Context where 

import Control.Monad.Except -- (MonadError)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Functor.Product (Product(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import AST 
import Types 
import Analysis.Error

data Ctx = Ctx
  { vars :: Map Text Ty,
    funs :: Map Text FunctionTy,
    dists :: Map Text FunctionTy,
    knownCards :: Set Text,
    vardom :: Map Text VarDomain
  } deriving Show

type MonadTyCtx m = (MonadState Ctx m, MonadError TypeError m)

lookupVar ::
  (MonadTyCtx m) =>
  Text ->
  m Ty
lookupVar name = do
  varCtx <- vars <$> get
  case M.lookup name varCtx of
    Nothing -> throwError $ UnBoundVarIdent name []
    Just ty -> return ty

lookupFun ::
  (MonadTyCtx m) =>
  Text ->
  m FunctionTy
lookupFun name = do
  funCtx <- funs <$> get
  case M.lookup name funCtx of
    Nothing -> throwError $ UnBoundFunctionIdent name []
    Just ty -> return ty

lookupDist ::
  (MonadTyCtx m) =>
  Text ->
  m FunctionTy
lookupDist name = do
  distsCtx <- dists <$> get
  case M.lookup name distsCtx of
    Nothing -> throwError $ UnBoundDistIdent name []
    Just ty -> return ty

insertTy ::
  forall m.
  (MonadTyCtx m) =>
  Text ->
  VarDomain -> 
  Ty ->
  m ()
insertTy name vd ty = do
  Ctx vars funs dists cards vardom <- get
  let vars' = M.insert name ty vars
  let vardom' = M.insert name vd vardom 
  put $ Ctx vars' funs dists cards vardom'
