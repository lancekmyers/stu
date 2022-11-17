{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Analysis (prettyError, checkModel, buildCtx, Ctx, ctxFromSigs, checkFunDef, checkLib) where

import AST
    ( Distribution(..),
      Library(Library),
      Model(..),
      ModelStmt(..),
      VarDomain(Data, Val, Param) )
-- (MonadReader)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Strict (gets)
import Control.Monad (when, forM)
import Data.Functor.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Types
import Control.Comonad.Trans.Cofree ( CofreeF((:<)), Cofree, CofreeT, headF, cofree )
import Text.Megaparsec.Pos (SourcePos (..), unPos)
import Control.Comonad.Identity (Identity (runIdentity, Identity))
import Data.Functor.Compose (Compose(getCompose, Compose))
import Data.Maybe (fromMaybe, mapMaybe)
import Analysis.Error ( TypeError(..), blame, prettyError )
import Analysis.Context
import Analysis.Expr
import Analysis.FunDef (checkFunDef) 
import Analysis.Distribution (inferTyDist) 
import Analysis.DistDef (checkDistDef)

cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project

stmtHandler ::
  (MonadError TypeError m) =>
  Text ->
  m a ->
  m a
stmtHandler name m = catchError m (throwError . BadStmt name)

checkModelStmt ::
  forall m a.
  (MonadTyCtx m) =>
  ModelStmt SourcePos ->
  m (ModelStmt Ty)
checkModelStmt (ValStmt name ty val) = stmtHandler name $ do
  val' <- inferTy val
  let ty' = cofreeHead val'
  let err = ExpectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) (throwError err)
  insertTy name Val ty 
  return (ValStmt name ty val')
checkModelStmt (ParamStmt name ty dist bij) = stmtHandler name $ do
  Distribution dname args ty' (bd, _) <- inferTyDist dist
  let err = ExpectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) (throwError err)
  insertTy name Param ty
  let br_sh = shDiff (shape ty') (shape ty)
  let bij' = fmap (const ty') <$> bij
  -- ensure bij' is a known bijector of the right type  
  let dist' = Distribution dname args ty (bd, br_sh)
  return (ParamStmt name ty dist' bij')
checkModelStmt (ObsStmt name dist) = stmtHandler name $ do
  Distribution dname args ty' (bd, _) <- inferTyDist dist
  ty <- lookupVar name
  let err = ExpectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) (throwError err)
  insertTy name Data ty
  let br_sh =  shDiff (shape ty')  (shape ty) 
  let dist' = Distribution dname args ty (bd, br_sh)
  return (ObsStmt name dist')

checkModel ::
  forall m.
  (MonadTyCtx m, MonadError TypeError m) =>
  Model SourcePos ->
  m (Model Ty)
checkModel (Model stmts) = Model <$> forM stmts checkModelStmt


allSame :: Eq a => [a] -> Bool
allSame xs = and $ zipWith (==) xs (tail xs)

checkLib :: forall m. (MonadTyCtx m, MonadError TypeError m) =>
  Library SourcePos ->
  m (Library Ty)
checkLib (Library funs dists) = do 
  funs' <- traverse checkFunDef funs
  dists' <- traverse checkDistDef dists
  return $ Library funs' dists'