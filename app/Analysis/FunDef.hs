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
import Analysis.Context (Ctx (vars), MonadTyCtx, insertFun, insertTy, introCards, introCardsFromTy, validateType, introArgs)
import Analysis.Error
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
import Types (FunctionTy (FunctionTy), Ty, broadcastsTo)
import Debug.Trace (trace)
import Util (SrcSpan)

cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project


checkFunDefs :: forall m. (MonadError TypeError m, MonadState Ctx m) => [FunDef SrcSpan] -> m [FunDef Ty]
checkFunDefs funs = mapM go funs
  where
    go :: FunDef SrcSpan -> m (FunDef Ty)
    go defn = do
      ctx <- get
      defn'@(FunDef name args ret body) <- runReaderT (checkFunDef defn) ctx
      insertFun name (FunctionTy args ret)
      ctx' <- get
      -- trace (show ctx') (pure ())
      return defn'

checkFunDef ::
  (MonadTyCtx m) =>
  FunDef SrcSpan -> 
  m (FunDef Ty)
checkFunDef (FunDef name args ret body) = blameStmt name $
  local (mconcat $ introCardsFromTy . snd <$> args) $ 
  local (introArgs args) $ do
    (ret', body') <- checkFunBody body
    let err = expectedGot ret ret'
    when (not $ ret' == ret) (throwError err)
    -- insertFun name (FunctionTy args ret)
    return $ FunDef name args ret' body'


checkFunBody ::
  (MonadTyCtx m) =>
  FunBody SrcSpan ->
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
  let err = expectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) (throwError err)
  (retTy, rest') <- checkFunBody rest
  return (retTy, FunLetIn name ty val' rest')
checkFunBody (FunRet expr) = do
  ty <- inferTy expr
  return (cofreeHead ty, FunRet ty)
