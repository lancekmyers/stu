{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analysis.Expr where

import AST (Expr, ExprF (..), Elaboration, Parsing)
import Analysis.Context
  ( MonadTyCtx,
    annotateVarDomain,
    lookupFun,
    lookupVar,
  )
import Analysis.Error
  ( badFunApp,
    binOpErr,
    invalidFold,
    invalidScan,
    nonHomogenousArrayLit,
    otherErr,
  )
import Control.Comonad.Identity (Identity (Identity, runIdentity))
import Control.Comonad.Trans.Cofree (CofreeF (..), Cofree, CofreeT (..), headF)
import Control.Monad (when)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Foldable
  ( Corecursive (embed),
    Recursive (para, project),
    cataA
  )
import Data.List (sort)
import qualified Data.Text as T
import Types
  ( Card (CardN),
    ElTy (..),
    FunctionTy (..),
    Ty (Ty),
    broadcast,
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
import Optics (_2, over)


-- inferTy ::
--   forall m.
--   (MonadTyCtx m) =>
--   Cofree (ExprF Parsing) SrcSpan ->
--   m (Cofree (ExprF Elaboration) Ty)
-- inferTy = para (go . runIdentity . getCompose)
--   where
--     {-
--     CofreeF ExprF SrcSpan 
--         (Cofree ExprF SrcSpan, m (Cofree ExprF Ty)) ->
--       m (Cofree ExprF Ty)
--     -}
--     go ::
--       CofreeF (ExprF _1) SrcSpan 
--         (Cofree (ExprF _2) SrcSpan, m (Cofree (ExprF _3) Ty)) 
--       -> m (Cofree (ExprF _4) Ty)
--     go (loc :< exp') = do
--       let embed' = embed . Compose . Identity
--       let foo = fmap (runIdentity . getCompose . project) . snd <$> exp'
--       let exp = fmap (\(ty :< _) -> ty) <$> foo
--       ty <- alg loc exp
--       baz <- sequence (fmap (\(_ :< e) -> e) <$> foo)
--       let qux = ((\e -> embed' $ ty :< e) <$> baz) :: ExprF (Cofree ExprF Ty)
--       qux' <- annotateVarDomain loc qux
--       return . embed' $ ty :< qux'

-- alg :: MonadTyCtx m => SrcSpan -> ExprF Elaboration (m Ty) -> m Ty
-- alg loc (ArithF binop t1 t2) = do
--   ty_lhs@(Ty sh_lhs el_lhs p1) <- t1
--   ty_rhs@(Ty sh_rhs el_rhs p2) <- t2
--   when (el_lhs /= el_rhs) $ binOpErr binop loc ty_lhs ty_rhs
--   case broadcast sh_lhs sh_rhs of
--     Nothing -> binOpErr binop loc ty_lhs ty_rhs
--     Just sh -> return $ Ty sh el_lhs (joinSrcSpan <$> p1 <*> p2)
-- alg loc (VarF name _) = lookupVar (Just loc) name
-- alg loc (FunAppF fname arg_tys) = do
--   fty <- lookupFun (Just loc) fname
--   arg_tys' <- sequenceA arg_tys
--   case unify arg_tys' fty of
--     Left _ -> badFunApp fname loc arg_tys' fty
--     Right (_, ret) -> return ret
-- alg loc (TransposeF x_ty perm) = do
--   Ty sh elTy _ <- x_ty
--   let isPerm = sort perm == [0 .. shRank sh]
--   if not isPerm
--     then otherErr "Invalaid permutation given to transpose"
--     else return $ Ty (shFromList $ (shToList sh !!) <$> perm) elTy (Just loc)
-- alg loc (FoldF fname x0 xs_ty) = do
--   -- f : (x : [..n]t, y : [..m]t') -> [..n]t
--   -- x0 : [..n]t
--   -- xs : [..m',..m]t'
--   xs_ty'@(Ty xs_sh elTy _) <- xs_ty
--   x0_ty'@(Ty x0_sh x0_el_ty _) <- x0
--   fty <- lookupFun (Just loc) fname
--   (prefix, sh) <- case fty of
--     FunctionTy [(_, t1), (_, t2)] ret
--       | ret == t1 -> case x0_sh `shDiff'` (shape t1) of
--           Just p -> pure (p, shape t1)
--           Nothing -> invalidFold loc fty x0_ty' xs_ty'
--     _ -> invalidFold loc fty x0_ty' xs_ty'

--   Ty ret_sh el_ret _ <- case unify [Ty x0_sh x0_el_ty Nothing, Ty sh elTy Nothing] fty of
--     Left _ -> invalidFold loc fty x0_ty' xs_ty'
--     Right (_, ret) -> return ret
--   return $ Ty (prefix <> ret_sh) el_ret (Just loc)
-- alg loc (ScanF fname x0_ty xs_ty) = do
--   -- (b -> a -> b) -> b -> [a] -> [b]
--   -- f : (x : [..n]t, y : [..m]t') : [..n]t
--   -- x0 : [..n]t
--   -- xs : [..m', ..m]t'
--   -- -------
--   --    : [..m', ..n]t
--   fty <- lookupFun Nothing fname
--   x0_ty'@(Ty x0_sh x0_el _) <- x0_ty
--   xs_ty'@(Ty xs_sh xs_el _) <- xs_ty
--   sh <- case fty of
--     FunctionTy [(_, t1), (_, t2)] ret_ty
--       | ret_ty == t1 -> case x0_sh `shDiff'` (shape t1) of
--           Nothing -> invalidScan loc fty x0_ty' xs_ty'
--           Just p -> pure $ p <> shape t2
--       | otherwise -> invalidScan loc fty x0_ty' xs_ty'
--     _ -> invalidScan loc fty x0_ty' xs_ty'
--   Ty ret_sh ret_el _ <- case unify
--     [ Ty x0_sh x0_el Nothing,
--       Ty sh xs_el Nothing
--     ]
--     fty of
--     Right (_, ret_ty) -> pure ret_ty
--     Left _ -> otherErr "The scanning function has the wrong type"
--   return $ Ty (xs_sh <> ret_sh) ret_el (Just loc)
-- alg loc (LitReal _) = return $ Ty [] REAL (Just loc)
-- alg loc (LitInt _) = return $ Ty [] INT (Just loc)
-- alg loc (LitArray []) = otherErr "Cannot infer type of empty tensor"
-- alg loc (LitArray tys) = do
--   tys' <- sequenceA tys
--   if and $ zipWith (==) tys' (tail tys')
--     then
--       let (Ty sh el _) = head tys'
--        in return $ Ty (shCons (CardN $ length tys') sh) el (Just loc)
--     else nonHomogenousArrayLit tys'


inferTy :: MonadTyCtx m => Expr Parsing -> m (Expr Elaboration)
inferTy = fmap snd . cataA (alg' . runIdentity . getCompose)

alg' :: MonadTyCtx m
  => CofreeF 
      (ExprF Parsing) 
      SrcSpan  
      (m (SrcSpan, Expr Elaboration))
  -> m (SrcSpan, Expr Elaboration)
alg' (loc :< x) = case fmap (over _2 (runIdentity . runCofreeT)) <$>  x of 
  VarF name -> do 
    ty <- lookupVar (Just loc) name 
    let var = VarF name :: ExprF Elaboration (Expr Elaboration)
    return (loc, CofreeT . Identity $ ty :< var)  
  -- 
  ArithF binop lhs rhs -> do 
    (loc_lhs, ty_lhs :< x_lhs) <- lhs 
    (loc_rhs, ty_rhs :< x_rhs) <- rhs 
    let Ty sh_lhs el_lhs _ = ty_lhs 
    let Ty sh_rhs el_rhs _ = ty_rhs
    when (el_lhs /= el_rhs) $ binOpErr binop loc ty_lhs ty_rhs
    case broadcast sh_lhs sh_rhs of
      Nothing -> binOpErr binop loc ty_lhs ty_rhs
      Just sh -> do 
        let ty = Ty sh el_lhs Nothing
        let rhs = mkCofree ty_lhs x_rhs 
        let lhs = mkCofree ty_lhs x_lhs 
        return $ (loc, mkCofree ty (ArithF binop lhs rhs))
  -- 
  (FunAppF fname args) -> do
    fty <- lookupFun (Just loc) fname
    args' <- sequenceA args 
    let arg_tys = map (headF . snd) args'
    let args'' = map (CofreeT . Identity . snd) args'
    case unify arg_tys fty of
      Left _ -> badFunApp fname loc arg_tys fty
      Right (_, ret) -> return (loc, mkCofree ret $ FunAppF fname args'')
  --
  (TransposeF x perm) -> do 
    (loc_x, (Ty sh elTy _) :< x)  <- x
    let isPerm = sort perm == [0 .. shRank sh]
    let ty = Ty (shFromList $ (shToList sh !!) <$> perm) elTy Nothing
    let x' = mkCofree ty x
    if not isPerm
      then otherErr "Invalaid permutation given to transpose"
      else return $ (loc, mkCofree ty $ TransposeF x' perm)
  --
  (FoldF fname x0 xs) -> do
    -- f : (x : [..n]t, y : [..m]t') -> [..n]t
    -- x0 : [..n]t
    -- xs : [..m',..m]t'
    (xs_loc, xs_ty@(Ty xs_sh elTy _) :< xs') <- xs
    (x0_loc, x0_ty@(Ty x0_sh x0_elTy _) :< x0') <- x0
    fty <- lookupFun Nothing fname
    (prefix, sh) <- case fty of
      FunctionTy [(_, t1), (_, t2)] ret
        | ret == t1 -> case x0_sh `shDiff'` (shape t1) of
            Just p -> pure (p, shape t1)
            Nothing -> invalidFold loc fty x0_ty xs_ty
      _ -> invalidFold loc fty x0_ty xs_ty
    Ty ret_sh el_ret _ <- case unify [Ty x0_sh x0_elTy Nothing, Ty sh elTy Nothing] fty of
      Left _ -> invalidFold loc fty x0_ty xs_ty
      Right (_, ret) -> return ret
    let ty = Ty (prefix <> ret_sh) el_ret Nothing
    let xs'' = mkCofree xs_ty xs'
    let x0'' = mkCofree x0_ty x0'
    return $ (loc, mkCofree ty $ FoldF fname x0'' xs'')
  (ScanF fname x0 xs) -> do
    -- (b -> a -> b) -> b -> [a] -> [b]
    -- f : (x : [..n]t, y : [..m]t') : [..n]t
    -- x0 : [..n]t
    -- xs : [..m', ..m]t'
    -- -------
    --    : [..m', ..n]t
    (xs_loc, xs_ty@(Ty xs_sh xs_elTy _) :< xs') <- xs
    (x0_loc, x0_ty@(Ty x0_sh x0_elTy _) :< x0') <- x0
    fty <- lookupFun Nothing fname
    sh <- case fty of
      FunctionTy [(_, t1), (_, t2)] ret_ty
        | ret_ty == t1 -> case x0_sh `shDiff'` (shape t1) of
            Nothing -> invalidScan loc fty x0_ty xs_ty
            Just p -> pure $ p <> shape t2
        | otherwise -> invalidScan loc fty x0_ty xs_ty
      _ -> invalidScan loc fty x0_ty xs_ty
    Ty ret_sh ret_el _ <- case unify
      [ Ty x0_sh x0_elTy Nothing,
        Ty sh xs_elTy Nothing
      ]
      fty of
      Right (_, ret_ty) -> pure ret_ty
      Left _ -> otherErr "The scanning function has the wrong type"
    let ty = Ty (xs_sh <> ret_sh) ret_el (Just loc)
    let xs'' = mkCofree xs_ty xs'
    let x0'' = mkCofree x0_ty x0' 
    return $ (loc, mkCofree ty $ ScanF fname x0'' xs'')

  (LitReal x) -> do 
    let ty = Ty [] REAL Nothing 
    return $ (loc, mkCofree ty $ LitReal x)
  (LitInt x) -> do 
    let ty = Ty [] INT Nothing 
    return $ (loc, mkCofree ty $ LitInt x)
  (LitArray [])  -> otherErr "Cannot infer type of empty tensor"
  (LitArray xs) -> do
    xs' <- sequenceA xs 
    let xs_tys = map (headF . snd) xs'
    let xs'' = map (CofreeT . Identity . snd) xs'

    case and $ zipWith (==) xs_tys (tail xs_tys) of 
      False -> nonHomogenousArrayLit xs_tys
      True -> do 
        let (Ty sh el _) = head xs_tys
        let ty = Ty (shCons (CardN $ length xs_tys) sh) el Nothing
        return $ (loc, mkCofree ty $ LitArray xs'')


mkCofree a x =  CofreeT . Identity $ (a :< x)
