{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}

module Analysis.DistDef where

-- import Control.Monad.State.Strict (gets, withStateT, MonadState (get))

import AST
  ( DistDef (DistDef),
    Distribution (Distribution),
    SampleBody (..),
    VarDomain (Local, Bound),
    FunDef (FunDef)
  )
import Analysis.Context (MonadTyCtx, insertDist, insertTy, validateType, introCards, introCardsFromTy)
import Analysis.Distribution (inferTyDist)
import Analysis.Error
import Analysis.Expr (inferTy)
import Analysis.FunDef (checkFunDefs, checkFunDef)
import Control.Comonad.Identity (Identity (runIdentity))
import Control.Comonad.Trans.Cofree (Cofree, headF)
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Data.Functor.Compose (Compose (getCompose))
import Data.Functor.Foldable (Recursive (project))
import qualified Data.Set as S 
import qualified Data.Vector as V
import Types (FunctionTy (FunctionTy), Ty, shape, broadcastsTo, Shape (getVec))
import Control.Monad.Reader.Class (MonadReader(local))
import Control.Monad.State.Strict (MonadState (get))
import Analysis.Context (Ctx)
import Control.Monad.Reader (runReaderT, MonadReader (ask))
import Util (SrcSpan)

cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project

checkDistDefs :: forall m. (MonadError TypeError m, MonadState Ctx m) => [DistDef SrcSpan] -> m [DistDef Ty]
checkDistDefs dists = traverse go dists
  where
    go :: DistDef SrcSpan -> m (DistDef Ty)
    go dist = do
      ctx <- get
      dist'@(DistDef name args eventTy lpdf sample bij) <- 
        runReaderT (checkDistDef dist) ctx
      insertDist name (FunctionTy args eventTy) (const () <$> bij)
      return dist'

checkDistDef ::
  (MonadTyCtx m) =>
  DistDef SrcSpan ->
  m (DistDef Ty)
checkDistDef (DistDef name args eventTy lpdf sample bij) = do
  ctx <- ask   
  lpdf' <- checkFunDef lpdf
  let insertArgs = mconcat [insertTy x Bound t . introCardsFromTy t | (x, t) <- args]
  (eventTy', sample') <- local insertArgs $ checkSample sample
  let err = expectedGot eventTy eventTy'
  when (eventTy' /= eventTy) $ throwError err
  let bij' = (const eventTy) <$> bij
  return $ DistDef name args eventTy lpdf' sample' bij'

checkSample :: (MonadTyCtx m) => SampleBody SrcSpan -> m (Ty, SampleBody Ty)
checkSample (SampleRet val) = do
  val' <- inferTy val
  let ty' = cofreeHead val'
  return (ty', SampleRet val')
checkSample (SampleIn name ty dist rest) = local (insertTy name Local ty) $ do
  validateType ty
  dist'@(Distribution _ _ ty' batch_info) <- inferTyDist dist
  let err = expectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) (throwError err)
  (eventTy, rest') <- checkSample rest
  return (eventTy, SampleIn name ty dist' rest')
checkSample (SampleUnifIn name ty rest) = local (insertTy name Local ty) $ do
  validateType ty
  (eventTy, rest') <- checkSample rest
  return (eventTy, SampleUnifIn name ty rest')
checkSample (SampleLetIn name ty val rest) = local (insertTy name Local ty) $ do
  validateType ty
  val' <- inferTy val
  let ty' = cofreeHead val'
  let err = expectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) (throwError err)
  (eventTy, rest') <- checkSample rest
  return (eventTy, SampleLetIn name ty val' rest')
