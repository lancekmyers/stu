{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Analysis (checkModel, buildCtx, Ctx, ctxFromSigs, checkFunDef, checkLib) where

import AST
  ( Distribution (..),
    Library (Library),
    Model (..),
    ModelStmt (..),
    VarDomain (Data, Param, Val),
  )
-- import Data.Maybe (fromMaybe, mapMaybe)

import Analysis.Context
  ( Ctx,
    MonadTyCtx,
    buildCtx,
    ctxFromSigs,
    insertTy,
    lookupVar,
    validateType,
  )
import Analysis.DistDef (checkDistDef, checkDistDefs)
import Analysis.Distribution (inferTyDist)
import Analysis.Error
import Analysis.Expr (inferTy)
import Analysis.FunDef (checkFunDef, checkFunDefs)
import Control.Comonad.Identity (Identity (..))
import Control.Comonad.Trans.Cofree (Cofree, headF)
import Control.Monad (forM, when)
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT))
import Control.Monad.State.Strict (MonadState (get), modify)
import Control.Monad.Validate (MonadValidate)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Foldable (Recursive (project))
import Data.Text (Text)
import Debug.Trace (trace)
import Types (Ty, broadcastsTo, shDiff, shape)
import Util (SrcSpan)

cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project

checkModelStmt ::
  forall m a.
  (MonadTyCtx m) =>
  ModelStmt SrcSpan ->
  m (ModelStmt Ty)
checkModelStmt (ValStmt name ty val) =
  local (insertTy name Val ty) $ do
    validateType ty
    val' <- inferTy val
    let ty' = cofreeHead val'
    -- let err = expectedGot ty ty'
    when (not $ ty' `broadcastsTo` ty) $ doesNotMatchReturnType ty ty'
    return (ValStmt name ty val')
checkModelStmt (ParamStmt name ty dist bij) =
  local (insertTy name Param ty) $ do
    validateType ty
    Distribution dname args ty' (bd, _) <- inferTyDist dist
    -- let err = expectedGot ty ty'
    when (not $ ty' `broadcastsTo` ty) $ doesNotMatchReturnType ty ty'
    let br_sh = shDiff (shape ty') (shape ty)
    let bij' = fmap (const ty') <$> bij
    -- ensure bij' is a known bijector of the right type
    let dist' = Distribution dname args ty (bd, br_sh)
    return (ParamStmt name ty dist' bij')
checkModelStmt (ObsStmt name dist) = do
  Distribution dname args ty' (bd, _) <- inferTyDist dist
  ty <- lookupVar Nothing name
  -- let err = expectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) $ doesNotMatchReturnType ty ty'
  let br_sh = shDiff (shape ty') (shape ty)
  let dist' = Distribution dname args ty (bd, br_sh)
  return (ObsStmt name dist')

checkModel ::
  forall m.
  (MonadState Ctx m, MonadValidate TypeError m) =>
  Model SrcSpan ->
  m (Model Ty)
checkModel (Model stmts) = Model <$> traverse go stmts
  where
    go :: ModelStmt SrcSpan -> m (ModelStmt Ty)
    go stmt =
      get >>= runReaderT (checkModelStmt stmt) >>= \x -> case x of
        ObsStmt _ _ -> pure x
        (ParamStmt name ty dist bij) -> modify (insertTy name Param ty) >> pure x
        (ValStmt name ty val) -> modify (insertTy name Val ty) >> pure x

allSame :: Eq a => [a] -> Bool
allSame xs = and $ zipWith (==) xs (tail xs)

checkLib ::
  forall m.
  (MonadState Ctx m, MonadValidate TypeError m) =>
  Library SrcSpan ->
  m (Library Ty)
checkLib (Library funs dists) = do
  funs' <- checkFunDefs funs
  dists' <- checkDistDefs dists
  return $ Library funs' dists'
