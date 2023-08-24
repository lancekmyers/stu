{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analysis.DistDef where

-- import Control.Monad.State.Strict (gets, withStateT, MonadState (get))

import AST
  ( DistDef (DistDef),
    Distribution (Distribution),
    FunDef (FunDef),
    SampleBody (..),
    Elaboration, Parsing
  )
import Analysis.Context (Ctx, MonadTyCtx, insertDist, insertTy, introArgs, introCards, introCardsFromTys, validateType)
import Analysis.Distribution (inferTyDist)
import Analysis.Error
import Analysis.Expr (inferTy)
import Analysis.FunDef (checkFunDef, checkFunDefs)
import Control.Comonad.Identity (Identity (runIdentity))
import Control.Comonad.Trans.Cofree (Cofree, headF)
import Control.Monad (when, unless)
import Control.Monad.Reader (MonadReader (ask), runReaderT)
import Control.Monad.Reader.Class (MonadReader (local))
import Control.Monad.State.Strict (MonadState (get))
import Control.Monad.Validate (MonadValidate)
import Data.Functor.Compose (Compose (getCompose))
import Data.Functor.Foldable (Recursive (project))
import qualified Data.Set as S
import qualified Data.Vector as V
import Types (FunctionTy (FunctionTy), Shape (getVec), Ty, broadcastsTo, shape)
import Util (SrcSpan)

cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project

checkDistDefs :: forall m. (MonadValidate TypeError m, MonadState Ctx m) => [DistDef Parsing] -> m [DistDef Elaboration]
checkDistDefs = traverse go
  where
    go :: DistDef Parsing -> m (DistDef Elaboration)
    go dist = do
      ctx <- get
      dist'@(DistDef name args eventTy lpdf sample bij) <-
        runReaderT (checkDistDef dist) ctx
      insertDist name (FunctionTy args eventTy) bij
      return dist'

checkDistDef ::
  forall m.
  (MonadTyCtx m) =>
  DistDef Parsing ->
  m (DistDef Elaboration)
checkDistDef (DistDef name args eventTy lpdf sample bij) = do
  ctx <- ask
  lpdf' <- checkFunDef lpdf
  (eventTy', sample') <- local (introArgs args) $ checkSample sample
  -- let err = expectedGot eventTy eventTy'
  when (eventTy' /= eventTy) $ doesNotMatchReturnType eventTy eventTy'
  let bij' = bij
  return $ DistDef name args eventTy lpdf' sample' bij'

checkSample :: (MonadTyCtx m) => SampleBody Parsing -> m (Ty, SampleBody Elaboration)
checkSample (SampleRet val) = do
  val' <- inferTy val
  let ty' = cofreeHead val'
  return (ty', SampleRet val')
checkSample (SampleIn name ty dist rest) = local (insertTy name  ty) $ do
  validateType ty
  dist'@(Distribution _ _ ty' batch_info) <- inferTyDist dist
  -- let err = expectedGot ty ty'
  unless (ty' `broadcastsTo` ty) $ doesNotMatchDeclaredType ty ty'
  (eventTy, rest') <- checkSample rest
  return (eventTy, SampleIn name ty dist' rest')
checkSample (SampleUnifIn name ty rest) = local (insertTy name  ty) $ do
  validateType ty
  (eventTy, rest') <- checkSample rest
  return (eventTy, SampleUnifIn name ty rest')
checkSample (SampleLetIn name ty val rest) = do
  validateType ty
  val' <- inferTy val
  let ty' = cofreeHead val'
  -- let err = expectedGot ty ty'
  unless (ty' `broadcastsTo` ty) $ doesNotMatchDeclaredType ty ty'
  (eventTy, rest') <- local (insertTy name ty) $ checkSample rest
  return (eventTy, SampleLetIn name ty val' rest')
