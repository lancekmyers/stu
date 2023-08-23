{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Analysis.Context where

import AST (Bijector, Decl (..), ExprF (VarF), Parsing, Name)
import Analysis.Error 
import Control.Monad.Except (MonadError (throwError), guard)
import Control.Monad.Validate 
import Data.Either (lefts, rights)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Types (FunctionTy, Ty(..), shape, Card (CardFV, CardBV, CardN), Shape (getVec))
import qualified Data.Vector as V 
import Control.Monad.State.Class (MonadState, modify, get, put)
import Control.Monad.Reader.Class (MonadReader, local, asks)
import qualified Data.Text as T
import Debug.Trace (trace)
import Util (SrcSpan)

data Ctx = Ctx
  { vars :: Map Name Ty,
    funs :: Map Text FunctionTy,
    dists :: Map Text (FunctionTy, Bijector),
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

type MonadTyCtx m = (MonadReader Ctx m, MonadValidate TypeError m)


lookupVar ::
  (MonadTyCtx m) =>
  Maybe SrcSpan -> 
  Text ->
  m Ty
lookupVar loc name = do
  varCtx <- asks vars
  case M.lookup name varCtx of 
    Nothing -> unBoundVarIdent loc name $ M.keys varCtx
    Just (Ty sh el _) -> return $ Ty sh el loc

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
  m (Bijector)
lookupDistDefaultBij loc name = do
  distsCtx <- asks dists
  case M.lookup name distsCtx of
    Nothing -> unBoundDistIdent loc name $ M.keys distsCtx
    Just (_, bij) -> return bij

insertTy ::
  Name ->
  Ty  ->
  Ctx -> Ctx
insertTy name ty (Ctx vars funs dists cards) = Ctx vars' funs dists cards
  where
    vars' = M.insert name ty vars
  
introArgs :: [(Text, Ty)] -> Ctx -> Ctx
introArgs args (Ctx vars funs dists cards) 
  = Ctx (vars <> vars') funs dists cards
  where
    vars' = M.fromList args

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
  Bijector ->
  m ()
insertDist name fty bij = do
  Ctx vars funs dists cards <- get
  let dists' = M.insert name (fty, bij) dists
  put $ Ctx vars funs dists' cards

buildCtx :: [Decl] -> Ctx
buildCtx decls = Ctx vars mempty mempty (S.map CardFV knownCards)
  where
    vars :: Map Text Ty
    vars = M.fromList $ mapMaybe go decls
      where
        go (DataDecl name ty) = Just (name, ty)
        go _ = Nothing
    knownCards :: Set Text
    knownCards = S.fromList $ mapMaybe go decls
      where
        go (CardDecl name) = Just name
        go (FactorDecl name) = Just name
        go _ = Nothing

-- is this doing the right thing
annotateVarDomain :: (MonadTyCtx m) => SrcSpan -> ExprF Parsing a -> m (ExprF Parsing a)
annotateVarDomain loc (VarF var) = do
  vars <- asks vars
  case M.lookup var vars of
    Nothing -> unBoundVarIdent (Just loc) var $ M.keys vars
    Just t -> return $ VarF var
annotateVarDomain loc x = return x

ctxFromSigs :: [Either (Text, FunctionTy) (Text, FunctionTy, Bijector)] -> Ctx
ctxFromSigs xs = Ctx mempty funs dists mempty
  where
    funs = M.fromList $ lefts xs
    dists = M.fromList [(name, (dty, bij)) | (name, dty, bij) <- rights xs]

validateType :: forall m. MonadTyCtx m => Ty -> m ()
validateType ty@(Ty _ _ (Just loc)) = do
  ctxCards <- asks knownCards
  let cards = getVec . shape $ ty 
  let isLitInt = \case {CardN _ -> True; _ -> False }
  let unknownCards = V.filter (not . (`S.member` ctxCards)) cards
  case filter (not . isLitInt) $ V.toList unknownCards of 
    [] -> pure ()
    CardFV c : _ ->  unBoundCardIdent (Just loc) c ([] :: [Text])
    CardBV c : _ ->  unBoundCardIdent (Just loc) c ([] :: [Text])

introCards :: S.Set Card -> Ctx -> Ctx 
introCards cs =  \ctx -> ctx {knownCards = S.union cs (knownCards ctx)}

introCardsFromTys :: Foldable f => f Ty -> Ctx -> Ctx
introCardsFromTys = introCards . foldMap go
  where 
    go :: Ty -> S.Set Card 
    go = S.fromList . V.toList . getVec . shape
