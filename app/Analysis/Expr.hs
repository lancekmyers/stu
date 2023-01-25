{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analysis.Expr where

import AST (ExprF (..))
import Analysis.Context
  ( MonadTyCtx,
    annotateVarDomain,
    lookupFun,
    lookupVar,
  )
import Analysis.Error (TypeError (..), blame)
import Control.Comonad.Identity (Identity (Identity, runIdentity))
import Control.Comonad.Trans.Cofree (Cofree, CofreeF (..))
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Foldable
  ( Corecursive (embed),
    Recursive (para, project),
  )
import Text.Megaparsec.Pos (SourcePos)
import Types
  ( Card (CardN),
    ElTy (..),
    Ty (Ty),
    broadcast,
    shCons,
    shUncons,
    unify,
  )

inferTy ::
  forall m.
  ( MonadTyCtx m,
    MonadError TypeError m
  ) =>
  Cofree ExprF SourcePos ->
  m (Cofree ExprF Ty)
inferTy = para (go . runIdentity . getCompose)
  where
    go ::
      CofreeF ExprF SourcePos (Cofree ExprF SourcePos, m (Cofree ExprF Ty)) ->
      m (Cofree ExprF Ty)
    go (loc :< exp) = do
      let embed' = embed . Compose . Identity
      let foo = fmap (runIdentity . getCompose . project) . snd <$> exp
      let exp = fmap (\(ty :< exp) -> ty) <$> foo
      ty <- catchError (alg exp) (throwError . blame loc)
      baz <- sequence (fmap (\(ty :< exp) -> exp) <$> foo)
      let qux = ((\e -> embed' $ ty :< e) <$> baz) :: ExprF (Cofree ExprF Ty)
      qux' <- annotateVarDomain qux
      return . embed' $ ty :< qux'

alg :: MonadTyCtx m => ExprF (m Ty) -> m Ty
alg (ArithF binop t1 t2) = do
  ty_lhs@(Ty sh_lhs el_lhs) <- t1
  ty_rhs@(Ty sh_rhs el_rhs) <- t2
  when (el_lhs /= el_rhs) . throwError $ BinOpElTyErr binop el_lhs el_rhs
  case broadcast sh_lhs sh_rhs of
    Nothing -> throwError $ BinOpShapeErr binop sh_lhs sh_rhs
    Just sh -> return $ Ty sh el_lhs
alg (VarF name _) = lookupVar name
alg (GatherF xs_ty is_ty) = do
  xs_ty'@(Ty xs_sh xs_el) <- xs_ty -- [k, l]real
  is_ty'@(Ty is_sh is_el) <- is_ty -- [i, j]#k
  case is_el of
    IND card -> case shUncons xs_sh of
      Just (k, ks) -> do
        when (card /= k) (throwError $ InvalidGather xs_ty' is_ty')
        let sh = is_sh <> ks
        return $ Ty sh xs_el
      Nothing -> throwError $ InvalidGather xs_ty' is_ty'
    _ -> throwError $ InvalidGather xs_ty' is_ty'
alg (FunAppF fname arg_tys) = do
  fty <- lookupFun fname
  arg_tys' <- sequenceA arg_tys
  case unify arg_tys' fty of
    Left _ -> throwError $ BadFunApp fname arg_tys' fty
    Right (_, ret) -> return ret
alg (LitReal _) = return $ Ty [] REAL
alg (LitInt _) = return $ Ty [] INT
alg (LitArray []) =
  throwError $
    OtherErr "Cannot infer type of empty tensor"
alg (LitArray tys) = do
  tys' <- sequenceA tys
  if and $ zipWith (==) tys' (tail tys')
    then
      let (Ty sh el) = head tys'
       in return $ Ty (shCons (CardN $ length tys') sh) el
    else throwError $ NonHomogenousArrayLit tys'