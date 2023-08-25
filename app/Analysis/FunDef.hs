{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Analysis.FunDef where

import AST
  ( FunBody (..),
    FunDef (FunDef),
    PrimApp (PrimApp),
    Elaboration, Parsing
  )
import Analysis.Context (Ctx (vars), MonadTyCtx, insertFun, insertTy, introCards, introCardsFromTys, validateType, introArgs)
import Analysis.Error
import Analysis.Expr (inferTy)
import Control.Comonad.Identity (Identity (..))
import Control.Comonad.Trans.Cofree (Cofree, headF)
import Control.Monad (forM_, when)
import Control.Monad.Reader (ReaderT (runReaderT), runReader)
import Control.Monad.Reader.Class (MonadReader (ask, local))
import Control.Monad.State.Strict (MonadState (get), withState)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Foldable (Recursive (project))
import qualified Data.Map.Strict as M
import Types (FunctionTy (FunctionTy), Ty, broadcastsTo)
import Debug.Trace (trace)
import Util (SrcSpan)
import Control.Monad.Validate (MonadValidate)

cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project


checkFunDefs :: forall m. (MonadValidate TypeError m, MonadState Ctx m) => [FunDef Parsing] -> m [FunDef Elaboration]
checkFunDefs funs = mapM go funs
  where
    go :: FunDef Parsing -> m (FunDef Elaboration)
    go defn = do
      ctx <- get
      defn'@(FunDef name args ret body) <- runReaderT (checkFunDef defn) ctx
      insertFun name (FunctionTy args ret)
      ctx' <- get
      -- trace (show ctx') (pure ())
      return defn'

checkFunDef ::
  (MonadTyCtx m) =>
  FunDef Parsing -> 
  m (FunDef Elaboration)
checkFunDef (FunDef name args ret body) = do
  let cards = map snd args
  (ret', body') <- 
    local (introCardsFromTys cards . introArgs args) $       
      checkFunBody body 
  when (not $ ret' == ret) $ doesNotMatchReturnType ret ret'
  return $ FunDef name args ret' body'


checkFunBody ::
  (MonadTyCtx m) =>
  FunBody Parsing ->
  m (Ty, FunBody Elaboration)
checkFunBody (LetPrimIn name ty (PrimApp fprim args) rest) =
  local (insertTy name ty) $ do
    validateType ty
    -- insertTy name Local ty
    args' <- traverse inferTy args
    (rty, rest') <- checkFunBody rest
    return $ (rty, LetPrimIn name ty (PrimApp fprim args') rest')
checkFunBody (FunLetIn name ty val rest) 
  = local (insertTy name ty) $ do
  validateType ty
  val' <- inferTy val 
  let ty' = cofreeHead val'
  when (not $ ty' `broadcastsTo` ty) $ doesNotMatchDeclaredType ty ty'
  (retTy, rest') <- checkFunBody rest
  return (retTy, FunLetIn name ty val' rest')
checkFunBody (FunRet expr) = do
  ty <- inferTy expr
  return (cofreeHead ty, FunRet ty)
