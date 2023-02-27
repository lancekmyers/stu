{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Analysis.Distribution (inferTyDist) where

import AST
  ( Distribution (..),
    Library (Library),
    Model (..),
    ModelStmt (..),
    VarDomain (Data, Param, Val),
  )
import Analysis.Context (MonadTyCtx, lookupDistTy)
import Analysis.Error
import Analysis.Expr (inferTy)
import Control.Comonad.Identity (Identity (runIdentity))
import Control.Comonad.Trans.Cofree (Cofree, headF)
import Control.Monad.Except (MonadError (..))
import Data.Functor.Compose (Compose (getCompose))
import Data.Functor.Foldable (Recursive (project))
import Text.Megaparsec.Pos (SourcePos)
import Types (Ty, shRank, unify)

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
  blame loc $ case unify arg_tys fty of
    Left _ -> throwError $ badDistr dname arg_tys fty
    Right (bd, ret) ->
      return $
        Distribution dname arg_ann ret (shRank <$> bd, br_sh)
