{-# LANGUAGE FlexibleContexts #-}
module Analysis.FunDef where 

import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Strict ( MonadState(get) ) 
import Control.Monad (when)
import Data.Functor.Foldable ( Recursive(project) ) 
import qualified Data.Map.Strict as M
import Types ( Ty, broadcastsTo, FunctionTy (FunctionTy) )
import Control.Comonad.Trans.Cofree ( Cofree, headF ) 
import Text.Megaparsec.Pos ( SourcePos ) 
import Control.Comonad.Identity (Identity (..))
import Data.Functor.Compose (Compose(..))
import Analysis.Error ( TypeError(ExpectedGot) ) 
import Analysis.Context ( MonadTyCtx, Ctx(vars), insertTy, insertFun )
import Analysis.Expr ( inferTy ) 
import AST
    ( FunBody(..),
      FunDef(FunDef),
      PrimApp(PrimApp),
      VarDomain(Local, Bound) ) 
      
cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project

deleteBoundVars :: (MonadTyCtx m) => m ()
deleteBoundVars = do 
    ctx <- get 
    let vars' = M.filter (\(_,vd) -> (vd == Bound) || (vd == Local)) (vars ctx)
    return ()

checkFunDef :: (MonadTyCtx m) => 
    FunDef SourcePos -> m (FunDef Ty)
checkFunDef (FunDef name args ret body) = do
    mapM_ (\(x,t) -> insertTy x Bound t) args 
    (ret', body') <- checkFunBody body 
    let err = ExpectedGot ret ret'
    when (not $ ret' == ret) (throwError err)
    deleteBoundVars 
    insertFun name (FunctionTy args ret)
    return $ FunDef name args ret' body' 


checkFunBody :: (MonadTyCtx m) => 
    FunBody SourcePos -> m (Ty, FunBody Ty)
checkFunBody (LetPrimIn name ty (PrimApp fprim args) rest) = do 
    insertTy name Local ty
    args' <- traverse inferTy args
    (rty, rest') <- checkFunBody rest
    return $ (rty, LetPrimIn name ty (PrimApp fprim args') rest')
checkFunBody (FunLetIn name ty val rest) = do 
    val' <- inferTy val
    let ty' = cofreeHead val'
    let err = ExpectedGot ty ty'
    when (not $ ty' `broadcastsTo` ty) (throwError err)
    insertTy name Local ty 
    (retTy, rest') <- checkFunBody rest
    return (retTy, FunLetIn name ty val' rest')
checkFunBody (FunRet expr) = do 
    ty <- inferTy expr
    return (cofreeHead ty, FunRet ty) 
