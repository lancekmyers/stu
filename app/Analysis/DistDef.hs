{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}

module Analysis.DistDef where

-- import Control.Monad.State.Strict (gets, withStateT, MonadState (get))

import AST
  ( DistDef (DistDef),
    Distribution (Distribution),
    SampleBody (..),
    VarDomain (Local), 
    FunDef (FunDef)
  )
import Analysis.Context (MonadTyCtx, insertDist, insertTy, validateType, introCards)
import Analysis.Distribution (inferTyDist)
import Analysis.Error (TypeError (ExpectedGot))
import Analysis.Expr (inferTy)
import Analysis.FunDef (checkFunDef, deleteBoundVars)
import Control.Comonad.Identity (Identity (runIdentity))
import Control.Comonad.Trans.Cofree (Cofree, headF)
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Data.Functor.Compose (Compose (getCompose))
import Data.Functor.Foldable (Recursive (project))
import qualified Data.Set as S 
import qualified Data.Vector as V
import Text.Megaparsec.Pos (SourcePos)
import Types (FunctionTy (FunctionTy), Ty (shape), broadcastsTo, Shape (getVec))
import Control.Monad.Reader.Class (MonadReader(local))
import Control.Monad.State.Strict (MonadState (get))
import Analysis.Context (Ctx)
import Control.Monad.Reader (runReaderT)

cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project

checkDistDefs :: forall m. (MonadError TypeError m, MonadState Ctx m) => [DistDef SourcePos] -> m [DistDef Ty]
checkDistDefs dists = traverse go dists
  where
    go :: DistDef SourcePos -> m (DistDef Ty)
    go dist = do
      ctx <- get
      dist'@(DistDef name args eventTy lpdf sample bij) <- runReaderT (checkDistDef dist) ctx
      insertDist name (FunctionTy args eventTy) (const () <$> bij)
      return dist'

checkDistDef ::
  (MonadTyCtx m) =>
  DistDef SourcePos ->
  m (DistDef Ty)
checkDistDef (DistDef name args eventTy lpdf sample bij) = do
  lpdf' <- checkFunDef lpdf
  (eventTy', sample') <- checkSample sample
  let err = ExpectedGot eventTy eventTy'
  when (eventTy' /= eventTy) $ throwError err
  let bij' = (const eventTy) <$> bij
  return $ DistDef name args eventTy lpdf' sample' bij'

checkSample :: (MonadTyCtx m) => SampleBody SourcePos -> m (Ty, SampleBody Ty)
checkSample (SampleRet val) = do
  val' <- inferTy val
  let ty' = cofreeHead val'
  deleteBoundVars
  return (ty', SampleRet val')
checkSample (SampleIn name ty dist rest) = local (insertTy name Local ty) $ do
  validateType ty
  dist'@(Distribution _ _ ty' batch_info) <- inferTyDist dist
  let err = ExpectedGot ty ty'
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
  let err = ExpectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) (throwError err)
  
  (eventTy, rest') <- checkSample rest
  return (eventTy, SampleLetIn name ty val' rest')

-- SampleRet <$> (inferTy val)
{-
data SampleBody ann
  = SampleIn    Text Ty (Distribution ann) (SampleBody ann)
  | SampleLetIn Text Ty (Expr ann) (SampleBody ann)
  | SampleRet    (Expr ann)
-}

-- FunDef name args ret body
{-
deleteBoundVars :: (MonadTyCtx m) => m ()
deleteBoundVars = do
    ctx <- get
    let vars' = M.filter (\(_,vd) -> (vd == Bound) || (vd == Local)) (vars ctx)
    return ()

checkFunDef :: (MonadTyCtx m) =>
    FunDef SourcePos -> m (FunDef Ty)
checkFunDef (FunDef name args ret body) = do
    mapM_ (\(x,t) -> insertTy x Bound t) args
    (ret', body') <- checkFunBody body
    let err = ExpectedGot ret ret'
    when (not $ ret' == ret) (throwError err)
    deleteBoundVars
    insertFun name (FunctionTy args ret)
    return $ FunDef name args ret' body'

checkFunBody :: (MonadTyCtx m) =>
    FunBody SourcePos -> m (Ty, FunBody Ty)
checkFunBody (LetPrimIn name ty (PrimApp fprim args) rest) = do
    insertTy name Local ty
    args' <- traverse inferTy args
    (rty, rest') <- checkFunBody rest
    return $ (rty, LetPrimIn name ty (PrimApp fprim args') rest')
checkFunBody (FunLetIn name ty val rest) = do
    val' <- inferTy val
    let ty' = cofreeHead val'
    let err = ExpectedGot ty ty'
    when (not $ ty' `broadcastsTo` ty) (throwError err)
    insertTy name Local ty
    (retTy, rest') <- checkFunBody rest
    return (retTy, FunLetIn name ty val' rest')
checkFunBody (FunRet expr) = do
    ty <- inferTy expr
    return (cofreeHead ty, FunRet ty)
-}
