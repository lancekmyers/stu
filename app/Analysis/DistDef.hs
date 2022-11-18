{-# LANGUAGE FlexibleContexts #-}
module Analysis.DistDef where 

import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Strict (gets, withStateT, MonadState (get))
import Control.Monad (when, forM)
import Data.Functor.Foldable ( Recursive(project) ) 
import qualified Data.Map.Strict as M
-- import Data.Text (Text)
import Types ( Ty, broadcastsTo, FunctionTy (FunctionTy) )
import Control.Comonad.Trans.Cofree ( CofreeF((:<)), Cofree, CofreeT, headF, cofree )
import Text.Megaparsec.Pos (SourcePos (..), unPos)
import Control.Comonad.Identity (Identity (runIdentity, Identity))
import Data.Functor.Compose (Compose(getCompose, Compose))
import Analysis.Error ( TypeError(..), blame, prettyError )
import Analysis.Context ( MonadTyCtx, Ctx(vars), insertTy, insertFun, insertDist )
import Analysis.Expr ( inferTy )
import Analysis.FunDef ( checkFunDef, deleteBoundVars  ) 
import AST
    ( PrimApp(PrimApp),
      FunDef(..),
      DistDef(..),
      VarDomain(Bound, Local), SampleBody (..), Distribution (Distribution) ) 
import Analysis.Distribution
import Types (FunctionTy)


cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project


checkDistDef :: (MonadTyCtx m) => 
    DistDef SourcePos -> m (DistDef Ty)
checkDistDef (DistDef name args eventTy lpdf sample bij) = do 
    lpdf' <- checkFunDef lpdf 
    (eventTy', sample') <- checkSample sample
    let err = ExpectedGot eventTy eventTy'
    when (eventTy' /= eventTy) $ throwError err 
    let bij' = (const eventTy) <$> bij
    insertDist name (FunctionTy args eventTy) (const () <$> bij)
    return $ DistDef name args eventTy lpdf' sample' bij'

checkSample :: (MonadTyCtx m) => SampleBody SourcePos -> m (Ty, SampleBody Ty) 
checkSample (SampleRet val) = do
    val' <- inferTy val
    let ty' = cofreeHead val' 
    deleteBoundVars
    return (ty', SampleRet val')
checkSample (SampleIn name ty dist rest) = do 
    dist'@(Distribution _ _ ty' batch_info) <- inferTyDist dist
    let err = ExpectedGot ty ty'
    when (not $ ty' `broadcastsTo` ty) (throwError err)
    insertTy name Local ty
    (eventTy, rest') <- checkSample rest
    return (eventTy, SampleIn name ty dist' rest') 
checkSample (SampleUnifIn name ty rest) = do 
    insertTy name Local ty 
    (eventTy, rest') <- checkSample rest
    return (eventTy, SampleUnifIn name ty rest') 
checkSample (SampleLetIn name ty val rest) = do 
    val' <- inferTy val
    let ty' = cofreeHead val'
    let err = ExpectedGot ty ty'
    when (not $ ty' `broadcastsTo` ty) (throwError err)
    insertTy name Local ty 
    (eventTy, rest') <- checkSample rest
    return (eventTy, SampleLetIn name ty val' rest') 

    --SampleRet <$> (inferTy val)
{-
data SampleBody ann 
  = SampleIn    Text Ty (Distribution ann) (SampleBody ann)
  | SampleLetIn Text Ty (Expr ann) (SampleBody ann) 
  | SampleRet    (Expr ann)
-}

-- FunDef name args ret body
{-
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
-}