{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Analysis.Context where 

import Control.Monad.Except ( MonadError(throwError) ) 
import Control.Monad.State.Strict ( MonadState(put, get), gets )
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import AST ( Decl(..), VarDomain(..), Bijector, ExprF (VarF) ) 
import Types ( FunctionTy, Ty ) 
import Analysis.Error ( TypeError(..) )
import Data.Either (lefts, rights)
import GHC.Generics (Generic)

data Ctx = Ctx
  { vars :: Map Text Ty,
    funs :: Map Text FunctionTy,
    dists :: Map Text (FunctionTy, Bijector ()),
    knownCards :: Set Text,
    vardom :: Map Text VarDomain
  } 

instance Semigroup Ctx where 
  (Ctx vars funs dists knownCards vardom) <> 
    (Ctx vars' funs' dists' knownCards' vardom') = 
      Ctx (vars <> vars') (funs <> funs') (dists <> dists') (knownCards <> knownCards') (vardom <> vardom')

instance Monoid Ctx where 
  mempty = Ctx mempty mempty mempty mempty mempty

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

lookupDistTy ::
  (MonadTyCtx m) =>
  Text ->
  m FunctionTy
lookupDistTy name = do
  distsCtx <- dists <$> get
  case M.lookup name distsCtx of
    Nothing -> throwError $ UnBoundDistIdent name []
    Just (ty, _) -> return ty

lookupDistDefaultBij ::
  (MonadTyCtx m) =>
  Text ->
  m (Bijector ())
lookupDistDefaultBij name = do
  distsCtx <- dists <$> get
  case M.lookup name distsCtx of
    Nothing -> throwError $ UnBoundDistIdent name []
    Just (_, bij) -> return bij

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

buildCtx :: [Decl] -> Ctx
buildCtx decls = Ctx vars mempty mempty knownCards varDoms
  where
    vars :: Map Text Ty
    vars = M.fromList $ mapMaybe go decls
      where
        go (DataDecl name ty) = Just (name, ty)
        go _ = Nothing
    varDoms = M.fromList $ mapMaybe go decls
      where
        go (DataDecl name ty) = Just (name, Data)
        go _ = Nothing
    knownCards :: Set Text
    knownCards = S.fromList $ mapMaybe go decls
      where
        go (CardDecl name) = Just name
        go (FactorDecl name) = Just name
        go _ = Nothing

annotateVarDomain :: (MonadTyCtx m) => ExprF a -> m (ExprF a)
annotateVarDomain (VarF name _) = do 
  vardoms <- gets vardom
  case M.lookup name vardoms of 
    Nothing -> throwError $ UnBoundVarIdent name []
    Just d  -> return $ VarF name d
annotateVarDomain x = return x

ctxFromSigs :: [Either (Text, FunctionTy) (Text, FunctionTy, Bijector ())] -> Ctx 
ctxFromSigs xs = Ctx mempty funs dists mempty mempty
  where
    funs = M.fromList $ lefts xs 
    dists = M.fromList [(name, (dty, bij)) | (name, dty, bij) <- rights xs]