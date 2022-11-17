{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Analysis.Distribution (inferTyDist) where

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
import Data.Functor.Foldable ( Recursive(project) )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Types ( shRank, unify, Ty )
import Control.Comonad.Trans.Cofree ( CofreeF((:<)), Cofree, CofreeT, headF, cofree )
import Text.Megaparsec.Pos (SourcePos (..), unPos)
import Control.Comonad.Identity (Identity (runIdentity, Identity))
import Data.Functor.Compose (Compose(getCompose, Compose))
import Data.Maybe (fromMaybe, mapMaybe)
import Analysis.Error ( TypeError(..), blame, prettyError )
import Analysis.Context ( lookupDistTy, MonadTyCtx )
import Analysis.Expr ( inferTy )
import Analysis.FunDef (checkFunDef) 

cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project

inferTyDist ::
  (MonadTyCtx m) =>
  Distribution SourcePos ->
  m (Distribution Ty) 
inferTyDist (Distribution dname args loc (_, br_sh)) = do
  fty <- lookupDistTy dname
  -- annotated expressions passed as arguments 
  arg_ann <- traverse inferTy args
  let arg_tys = map cofreeHead arg_ann
  case unify arg_tys fty of
    Left _ -> throwError $ Blame loc $ BadDistr dname arg_tys fty
    Right (bd, ret) -> return $
      Distribution dname arg_ann ret (shRank <$> bd, br_sh)
