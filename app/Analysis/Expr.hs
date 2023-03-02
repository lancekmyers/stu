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
import Analysis.Error -- (TypeError (..), blame)
import Control.Comonad.Identity (Identity (Identity, runIdentity))
import Control.Comonad.Trans.Cofree (Cofree, CofreeF (..))
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Foldable
  ( Corecursive (embed),
    Recursive (para, project),
  )
import Types
  ( Card (CardN),
    ElTy (..),
    Ty (Ty),
    broadcast,
    shCons,
    shUncons,
    unify,
  )
import Util (SrcSpan, joinSrcSpan)

inferTy ::
  forall m.
  ( MonadTyCtx m ) =>
  Cofree ExprF SrcSpan ->
  m (Cofree ExprF Ty)
inferTy = para (go . runIdentity . getCompose)
  where
    go ::
      CofreeF ExprF SrcSpan (Cofree ExprF SrcSpan, m (Cofree ExprF Ty)) ->
      m (Cofree ExprF Ty)
    go (loc :< exp) = do
      let embed' = embed . Compose . Identity
      let foo = fmap (runIdentity . getCompose . project) . snd <$> exp
      let exp = fmap (\(ty :< exp) -> ty) <$> foo
      ty <- alg loc exp
      baz <- sequence (fmap (\(ty :< exp) -> exp) <$> foo)
      let qux = ((\e -> embed' $ ty :< e) <$> baz) :: ExprF (Cofree ExprF Ty)
      qux' <- annotateVarDomain loc qux
      return . embed' $ ty :< qux'

alg :: MonadTyCtx m => SrcSpan -> ExprF (m Ty) -> m Ty
alg loc (ArithF binop t1 t2) = do
  ty_lhs@(Ty sh_lhs el_lhs p1) <- t1
  ty_rhs@(Ty sh_rhs el_rhs p2) <- t2
  when (el_lhs /= el_rhs) $ binOpErr binop loc ty_lhs ty_rhs
  case broadcast sh_lhs sh_rhs of
    Nothing -> binOpErr binop loc ty_lhs ty_rhs
    Just sh -> return $ Ty sh el_lhs (joinSrcSpan <$> p1 <*> p2)
alg loc (VarF name _) = lookupVar (Just loc) name
alg loc (GatherF xs_ty is_ty) = do
  xs_ty'@(Ty xs_sh xs_el _) <- xs_ty -- [k, l]real
  is_ty'@(Ty is_sh is_el _) <- is_ty -- [i, j]#k
  case is_el of
    IND card -> case shUncons xs_sh of
      Just (k, ks) -> do
        when (card /= k) (invalidGather loc xs_ty' is_ty')
        let sh = is_sh <> ks
        return $ Ty sh xs_el (Just loc)
      Nothing -> invalidGather loc xs_ty' is_ty'
    _ -> invalidGather loc xs_ty' is_ty'
alg loc (FunAppF fname arg_tys) = do
  fty <- lookupFun (Just loc) fname
  arg_tys' <- sequenceA arg_tys
  case unify arg_tys' fty of
    Left _ -> badFunApp fname loc arg_tys' fty
    Right (_, ret) -> return ret
alg loc (LitReal _) = return $ Ty [] REAL (Just loc) 
alg loc (LitInt _) = return $ Ty [] INT (Just loc)
alg loc (LitArray []) = otherErr "Cannot infer type of empty tensor"
alg loc (LitArray tys) = do
  tys' <- sequenceA tys
  if and $ zipWith (==) tys' (tail tys')
    then
      let (Ty sh el _) = head tys'
       in return $ Ty (shCons (CardN $ length tys') sh) el (Just loc)
    else nonHomogenousArrayLit tys' 
