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
    Elaboration, Parsing
  )
import Analysis.Context (MonadTyCtx, lookupDistTy)
import Analysis.Error
import Analysis.Expr (inferTy)
import Control.Comonad.Identity (Identity (runIdentity))
import Control.Comonad.Trans.Cofree (Cofree, headF)
import Control.Monad.Except (MonadError (..))
import Data.Functor.Compose (Compose (getCompose))
import Data.Functor.Foldable (Recursive (project))
import Types (Ty, shRank, unify)
import Util (SrcSpan)

cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project

inferTyDist :: forall m. 
  (MonadTyCtx m) =>
  Distribution Parsing ->
  m (Distribution Elaboration)
inferTyDist (Distribution dname args loc (_, br_sh)) = do
  fty <- lookupDistTy (Just loc) dname
  -- annotated expressions passed as arguments
  arg_ann <- traverse inferTy args 
  let arg_tys = (map cofreeHead) arg_ann 
  case unify arg_tys fty of
    Left _ -> badDistr dname loc arg_tys fty
    Right (bd, ret) ->
      return $
        Distribution dname arg_ann ret (shRank <$> bd, br_sh)
