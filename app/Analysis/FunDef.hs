{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analysis.FunDef where

import AST
  ( FunBody (..),
    FunDef (FunDef),
    PrimApp (PrimApp),
    VarDomain (Bound, Local),
  )
import Analysis.Context (Ctx (vars), MonadTyCtx, insertFun, insertTy, introCards, introCardsFromTy, validateType)
import Analysis.Error (TypeError (ExpectedGot))
import Analysis.Expr (inferTy)
import Control.Comonad.Identity (Identity (..))
import Control.Comonad.Trans.Cofree (Cofree, headF)
import Control.Monad (forM_, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (runReaderT), runReader)
import Control.Monad.Reader.Class (MonadReader (ask, local))
import Control.Monad.State.Strict (MonadState (get))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Foldable (Recursive (project))
import qualified Data.Map.Strict as M
import Text.Megaparsec.Pos (SourcePos (SourcePos))
import Types (FunctionTy (FunctionTy), Ty, broadcastsTo)

cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project

deleteBoundVars :: (MonadTyCtx m) => m ()
deleteBoundVars = do
  ctx <- ask
  let vars' = M.filter (\(_, vd) -> (vd == Bound) || (vd == Local)) (vars ctx)
  return ()

checkFunDefs :: forall m. (MonadError TypeError m, MonadState Ctx m) => [FunDef SourcePos] -> m [FunDef Ty]
checkFunDefs funs = traverse go funs
  where
    go :: FunDef SourcePos -> m (FunDef Ty)
    go defn@(FunDef name args ret body) = do
      ctx <- get
      defn' <- runReaderT (checkFunDef defn) ctx
      insertFun name (FunctionTy args ret)
      return defn'

checkFunDef ::
  (MonadTyCtx m) =>
  FunDef SourcePos ->
  m (FunDef Ty)
checkFunDef (FunDef name args ret body) = local
  (mconcat [insertTy x Bound t . introCardsFromTy t | (x, t) <- args])
  $ do
    (ret', body') <- checkFunBody body
    let err = ExpectedGot ret ret'
    when (not $ ret' == ret) (throwError err)
    deleteBoundVars
    -- insertFun name (FunctionTy args ret)
    return $ FunDef name args ret' body'

{-
forM_ args $ \(x, t) -> (insertTy x Bound t) >> (introCardsFromTy t)

(ret', body') <- checkFunBody body
let err = ExpectedGot ret ret'
when (not $ ret' == ret) (throwError err)
deleteBoundVars
insertFun name (FunctionTy args ret)
return $ FunDef name args ret' body'
-}
checkFunBody ::
  (MonadTyCtx m) =>
  FunBody SourcePos ->
  m (Ty, FunBody Ty)
checkFunBody (LetPrimIn name ty (PrimApp fprim args) rest) =
  local (insertTy name Local ty) $ do
    validateType ty
    -- insertTy name Local ty
    args' <- traverse inferTy args
    (rty, rest') <- checkFunBody rest
    return $ (rty, LetPrimIn name ty (PrimApp fprim args') rest')
checkFunBody (FunLetIn name ty val rest) = local (insertTy name Local ty) $ do
  validateType ty
  val' <- inferTy val
  let ty' = cofreeHead val'
  let err = ExpectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) (throwError err)
  (retTy, rest') <- checkFunBody rest
  return (retTy, FunLetIn name ty val' rest')
checkFunBody (FunRet expr) = do
  ty <- inferTy expr
  return (cofreeHead ty, FunRet ty)
