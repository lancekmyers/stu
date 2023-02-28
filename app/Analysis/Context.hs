{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Analysis.Context where

import AST (Bijector, Decl (..), ExprF (VarF), VarDomain (..))
import Analysis.Error 
import Control.Monad.Except (MonadError (throwError), guard)
import Control.Monad.State.Strict (MonadState (get, put), gets)
import Data.Either (lefts, rights)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Types (FunctionTy, Ty(..), shape, Card (CardFV, CardBV, CardN), Shape (getVec))
import qualified Data.Vector as V 
import Control.Monad.State.Class (modify)
import Control.Monad.Reader.Class (MonadReader (ask), asks)
import qualified Data.Text as T
import Debug.Trace (trace)
import Util (SrcSpan)

data Ctx = Ctx
  { vars :: Map Text (Ty, VarDomain),
    funs :: Map Text FunctionTy,
    dists :: Map Text (FunctionTy, Bijector ()),
    knownCards :: Set Card
  } 
instance Show Ctx where 
  show (Ctx vars funs dists knownCards) = "funs \n\t" <> funNames
    where 
      funNames = T.unpack (T.intercalate "\n\t" $ M.keys funs)


instance Semigroup Ctx where
  (Ctx vars funs dists knownCards)
    <> (Ctx vars' funs' dists' knownCards') =
      Ctx (vars <> vars') (funs <> funs') (dists <> dists') (knownCards <> knownCards')

instance Monoid Ctx where
  mempty = Ctx mempty mempty mempty mempty

type MonadTyCtx m = (MonadReader Ctx m, MonadError TypeError m)


lookupVar ::
  (MonadTyCtx m) =>
  Maybe SrcSpan -> 
  Text ->
  m Ty
lookupVar loc name = do
  varCtx <- asks vars
  case M.lookup name varCtx of 
    Nothing -> unBoundVarIdent loc name $ M.keys varCtx
    Just (ty, _vd) -> return ty

lookupFun ::
  (MonadTyCtx m) =>
  Maybe SrcSpan ->
  Text ->
  m FunctionTy
lookupFun loc name = do
  funCtx <- asks funs 
  -- trace (show $ (name, M.keys funCtx)) $ pure () 
  case M.lookup name funCtx of
    Nothing -> unBoundFunctionIdent loc name $ M.keys funCtx
    Just ty -> return ty

lookupDistTy ::
  (MonadTyCtx m) =>
  Maybe SrcSpan -> 
  Text ->
  m FunctionTy
lookupDistTy loc name = do
  distsCtx <- asks dists
  case M.lookup name distsCtx of
    Nothing -> unBoundDistIdent loc name $ M.keys distsCtx
    Just (ty, _) -> return ty

lookupDistDefaultBij ::
  (MonadTyCtx m) =>
  Maybe SrcSpan -> 
  Text ->
  m (Bijector ())
lookupDistDefaultBij loc name = do
  distsCtx <- asks dists
  case M.lookup name distsCtx of
    Nothing -> unBoundDistIdent loc name $ M.keys distsCtx
    Just (_, bij) -> return bij

insertTy ::
  Text ->
  VarDomain ->
  Ty  ->
  Ctx -> Ctx
insertTy name vd ty (Ctx vars funs dists cards) = Ctx vars' funs dists cards
  where vars' = M.insert name (ty, vd) vars
  
introArgs :: [(Text, Ty)] -> Ctx -> Ctx
introArgs args (Ctx vars funs dists cards) 
  = Ctx (vars <> vars') funs dists cards
  where
    vars' = M.fromList [ (x, (t, Bound)) | (x, t) <- args]

insertFun ::
  forall m.
  (MonadState Ctx m) =>
  Text ->
  FunctionTy ->
  m ()
insertFun name fty = do
  Ctx vars funs dists cards <- get
  let funs' = M.insert name fty funs
  put $ Ctx vars funs' dists cards

insertDist ::
  forall m.
  (MonadState Ctx m) =>
  Text ->
  FunctionTy ->
  Bijector () ->
  m ()
insertDist name fty bij = do
  Ctx vars funs dists cards <- get
  let dists' = M.insert name (fty, bij) dists
  put $ Ctx vars funs dists' cards

buildCtx :: [Decl] -> Ctx
buildCtx decls = Ctx vars mempty mempty (S.map CardFV knownCards)
  where
    vars :: Map Text (Ty, VarDomain)
    vars = M.fromList $ mapMaybe go decls
      where
        go (DataDecl name ty) = Just (name, (ty, Data))
        go _ = Nothing
    knownCards :: Set Text
    knownCards = S.fromList $ mapMaybe go decls
      where
        go (CardDecl name) = Just name
        go (FactorDecl name) = Just name
        go _ = Nothing

annotateVarDomain :: (MonadTyCtx m) => SrcSpan -> ExprF a -> m (ExprF a)
annotateVarDomain loc (VarF name _) = do
  vars <- asks vars
  case M.lookup name vars of
    Nothing -> unBoundVarIdent (Just loc) name $ M.keys vars
    Just (t, vd) -> return $ VarF name vd
annotateVarDomain loc x = return x

ctxFromSigs :: [Either (Text, FunctionTy) (Text, FunctionTy, Bijector ())] -> Ctx
ctxFromSigs xs = Ctx mempty funs dists mempty
  where
    funs = M.fromList $ lefts xs
    dists = M.fromList [(name, (dty, bij)) | (name, dty, bij) <- rights xs]

validateType :: forall m. MonadTyCtx m => Ty -> m ()
validateType ty@(Ty _ _ (Just loc)) = do
  ctxCards <- asks knownCards
  let cards = getVec . shape $ ty 
  let isLitInt = \(CardN _) -> True
  let unknownCards = V.filter (not . (`S.member` ctxCards)) cards
  case filter (not . isLitInt) $ V.toList unknownCards of 
    [] -> pure ()
    CardFV c : _ ->  unBoundCardIdent (Just loc) c ([] :: [Text])
    CardBV c : _ ->  unBoundCardIdent (Just loc) c ([] :: [Text])

introCards :: S.Set Card -> Ctx -> Ctx 
introCards cs =  \ctx -> ctx {knownCards = S.union cs (knownCards ctx)}

introCardsFromTy :: Ty -> Ctx -> Ctx
introCardsFromTy = introCards . S.fromList . V.toList . getVec . shape 
