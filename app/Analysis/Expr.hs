{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

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
import Data.List (sort)
import Data.Text (pack)
import Types
  ( Card (CardN),
    ElTy (..),
    FunctionTy (..),
    Ty (Ty),
    broadcast,
    rank,
    shCons,
    shDiff',
    shFromList,
    shRank,
    shToList,
    shUncons,
    shape,
    unify,
  )
import Util (SrcSpan, joinSrcSpan)

inferTy ::
  forall m.
  (MonadTyCtx m) =>
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
alg loc (ScatterAddF xs_ty is_ty) = do
  -- xs : [k]t
  -- is : [n]#k
  -- ------------
  --    : [n]real
  (Ty xs_sh xs_el _xs_loc) <- xs_ty
  (Ty is_sh is_el _is_loc) <- is_ty
  case (shToList xs_sh, shToList is_sh) of
    ([k], [_n])
      | is_el == (IND k) -> return $ Ty is_sh xs_el (Just loc)
      | otherwise -> otherErr "Scatter rhs must have elements of type of index of lhs"
    _ ->
      otherErr $
        "scatter expects vector arguments, was given tensor of nonzero rank"
alg loc (TransposeF x_ty perm) = do
  Ty sh elTy _ <- x_ty
  let isPerm = sort perm == [0 .. shRank sh]
  if not isPerm
    then otherErr "Invalaid gather given to transpose"
    else return $ Ty (shFromList $ (shToList sh !!) <$> perm) elTy (Just loc)
alg loc (FoldF fname x0 xs_ty) = do
  -- f : (x : [..n]t, y : [..m]t') -> [..n]t
  -- x0 : [..n]t
  -- xs : [..m',..m]t'
  Ty xs_sh elTy _ <- xs_ty
  Ty x0_sh x0_el_ty _ <- x0
  fty <- lookupFun (Just loc) fname
  {- I think that this is wrong, take shDiff' x0_sh (shape t1) -}
  {- or mayber take shDiff' xs_sh (shape t2) -}
  -- no, I've come back arround to this
  -- I replaces xs_sh with x0_sh
  -- This is almost certainly wrong
  (prefix, sh) <- case fty of
    FunctionTy [(_, t1), (_, t2)] ret
      | ret == t1 -> case x0_sh `shDiff'` (shape t1) of
          Just p -> pure (p, shape t1)
          Nothing -> otherErr "The folding function does not have the proper type"
    _ -> otherErr "The folding function does not have the proper type"

  Ty ret_sh el_ret _ <- case unify [Ty x0_sh x0_el_ty Nothing, Ty sh elTy Nothing] fty of
    Left _ -> otherErr "The folding function does not have the proper type"
    Right (_, ret) -> return ret
  return $ Ty (prefix <> ret_sh) el_ret (Just loc)
alg loc (ScanF fname x0_ty xs_ty) = do
  -- (b -> a -> b) -> b -> [a] -> [b]
  -- f : (x : [..n]t, y : [..m]t') : [..n]t
  -- x0 : [..n]t
  -- xs : [..m', ..m]t'
  -- -------
  --    : [..m', ..n]t
  fty <- lookupFun Nothing fname
  Ty x0_sh x0_el _ <- x0_ty
  Ty xs_sh xs_el _ <- xs_ty
  sh <- case fty of
    FunctionTy [(_, t1), (_, t2)] ret_ty
      | ret_ty == t1 -> case x0_sh `shDiff'` (shape t1) of
          Nothing -> otherErr "The scanning function has the wrong type"
          Just p -> pure $ p <> shape t2
      | otherwise -> otherErr "The scanning function has the wrong type"
    _ -> otherErr "The scanning function has the wrong type"
  Ty ret_sh ret_el _ <- case unify
    [ Ty x0_sh x0_el Nothing,
      Ty sh xs_el Nothing
    ]
    fty of
    Right (_, ret_ty) -> pure ret_ty
    Left _ -> otherErr "The scanning function has the wrong type"
  return $ Ty (xs_sh <> ret_sh) ret_el (Just loc)
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
