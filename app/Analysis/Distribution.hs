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
import Control.Monad.Except (MonadError(..))
import Data.Functor.Foldable ( Recursive(project) )
import Types ( shRank, unify, Ty )
import Control.Comonad.Trans.Cofree ( Cofree, headF ) 
import Text.Megaparsec.Pos (SourcePos)
import Control.Comonad.Identity (Identity (runIdentity))
import Data.Functor.Compose (Compose(getCompose))
import Analysis.Error ( TypeError(..) )
import Analysis.Context ( lookupDistTy, MonadTyCtx )
import Analysis.Expr ( inferTy )

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
