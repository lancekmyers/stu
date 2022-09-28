{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Analysis where

import AST
-- (MonadReader)
import Control.Monad.Except -- (MonadError)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Functor.Product (Product(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Vector as V
import Prettyprinter
import Types
import Control.Comonad.Trans.Cofree ( CofreeF((:<)) )
import qualified Control.Comonad.Cofree as Cofree (Cofree(..))
import Text.Megaparsec.Pos (SourcePos (..), unPos)
import Control.Comonad (extract)

data Ctx = Ctx
  { vars :: Map Text Ty,
    funs :: Map Text FunctionTy,
    dists :: Map Text FunctionTy,
    knownCards :: Set Text
  }

type MonadTyCtx m = (MonadState Ctx m, MonadError TypeError m)

lookupVar ::
  (MonadTyCtx m) =>
  Text ->
  m Ty
lookupVar name = do
  varCtx <- vars <$> get
  case M.lookup name varCtx of
    Nothing -> throwError $ UnBoundVarIdent name []
    Just ty -> return ty

lookupFun ::
  (MonadTyCtx m) =>
  Text ->
  m FunctionTy
lookupFun name = do
  funCtx <- funs <$> get
  case M.lookup name funCtx of
    Nothing -> throwError $ UnBoundFunctionIdent name []
    Just ty -> return ty

lookupDist ::
  (MonadTyCtx m) =>
  Text ->
  m FunctionTy
lookupDist name = do
  distsCtx <- dists <$> get
  case M.lookup name distsCtx of
    Nothing -> throwError $ UnBoundDistIdent name []
    Just ty -> return ty

insertTy ::
  forall m.
  (MonadTyCtx m) =>
  Text ->
  Ty ->
  m ()
insertTy name ty = do
  Ctx vars funs dists cards <- get
  let vars' = M.insert name ty vars
  put $ Ctx vars' funs dists cards

inferTy ::
  forall m.
  ( MonadTyCtx m,
    MonadError TypeError m
  ) =>
  ExprSrc ->
  m Ty
inferTy = cataA blame
  where
    -- proj (ann :< exp) = exp
    blame :: CofreeF ExprF SourcePos (m Ty) -> m Ty
    blame (loc :< exp) = catchError (alg exp) (throwError . Blame loc)
    
    alg :: ExprF (m Ty) -> m Ty
    alg (ArithF binop t1 t2) = do
      ty_lhs@(Ty sh_lhs el_lhs) <- t1
      ty_rhs@(Ty sh_rhs el_rhs) <- t2
      when (el_lhs /= el_rhs) . throwError $ BinOpElTyErr binop el_lhs el_rhs
      case broadcast sh_lhs sh_rhs of
        Nothing -> throwError $ BinOpShapeErr binop sh_lhs sh_rhs
        Just sh -> return $ Ty sh el_lhs
    alg (VarF name) = lookupVar name
    alg (GatherF xs_ty is_ty) = do
      xs_ty'@(Ty xs_sh xs_el) <- xs_ty -- [k, l]real
      is_ty'@(Ty is_sh is_el) <- is_ty -- [i, j]#k
      case is_el of
        IND card -> case V.uncons xs_sh of
          Just (k, ks) -> do
            when (card /= k) (throwError $ InvalidGather xs_ty' is_ty')
            let sh = is_sh <> ks
            return $ Ty sh xs_el
          Nothing -> throwError $ InvalidGather xs_ty' is_ty'
        _ -> throwError $ InvalidGather xs_ty' is_ty'
    alg (FunAppF fname arg_tys) = do
      fty <- lookupFun fname
      arg_tys' <- sequenceA arg_tys
      case unify arg_tys' fty of
        Left _ -> throwError $ BadFunApp fname arg_tys' fty
        Right ret -> return ret
    alg (LitReal    _) = return $ Ty [] REAL
    alg (LitInt     _) = return $ Ty [] INT
    alg (LitArray  []) = throwError $ 
      OtherErr "Cannot infer type of empty tensor"
    alg (LitArray tys) = do 
      tys' <- sequenceA tys
      if and $ zipWith (==) tys' (tail tys')
      then
        let (Ty sh el) = head tys'
        in return $ Ty (V.cons (CardN $ length tys') sh) el
      else throwError $ NonHomogenousArrayLit tys'



inferTyDist ::
  (MonadTyCtx m) =>
  Distribution SourcePos ->
  m Ty
inferTyDist (Distribution dname args) = do
  fty <- lookupDist dname
  arg_tys <- traverse inferTy args
  case unify arg_tys fty of
    Left _ -> throwError $ BadDistr dname arg_tys fty
    Right ret -> return ret

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
  m ()
checkModelStmt (ValStmt name ty val) = stmtHandler name $ do
  ty' <- inferTy val
  let err = ExpectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) (throwError err)
  insertTy name ty
checkModelStmt (ParamStmt name ty dist _) = stmtHandler name $ do
  ty' <- inferTyDist dist
  let err = ExpectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) (throwError err)
  insertTy name ty
checkModelStmt (ObsStmt name dist) = stmtHandler name $ do
  ty <- inferTyDist dist
  ty' <- lookupVar name
  let err = ExpectedGot ty ty'
  when (not $ ty' `broadcastsTo` ty) (throwError err)
  insertTy name ty

checkModel ::
  forall m.
  (MonadTyCtx m, MonadError TypeError m) =>
  Model SourcePos ->
  m ()
checkModel (Model stmts) = forM_ stmts checkModelStmt

allSame :: Eq a => [a] -> Bool
allSame xs = and $ zipWith (==) xs (tail xs)

-- should move to separate module

data TypeError
  = IncompatibleShapes Shape Shape
  | BadFunApp Text [Ty] FunctionTy
  | BadDistr Text [Ty] FunctionTy
  | BadStmt Text TypeError
  | BinOpShapeErr BinOp Shape Shape
  | BinOpElTyErr BinOp ElTy ElTy
  | InvalidGather Ty Ty
  | ExpectedGot Ty Ty
  | UnBoundFunctionIdent Text [Text]
  | UnBoundDistIdent Text [Text]
  | UnBoundVarIdent Text [Text]
  | NonHomogenousArrayLit [Ty]
  | Blame SourcePos TypeError 
  | OtherErr Text
  deriving (Show)

instance Pretty TypeError where
  pretty (IncompatibleShapes sh sh') =
    vsep
      [ "The shape",
        indent 2 $ prettyShape sh,
        "Does not broadcast with",
        indent 2 $ prettyShape sh'
      ]
  pretty (BadFunApp fname given fty) =
    vsep
      [ "The function" <+> pretty fname <+> "has type",
        indent 2 $ pretty fty,
        "But was provided arguments of type",
        indent 2 $ tupled $ pretty <$> given
      ]
  pretty (BadDistr dname given fty) =
    vsep
      [ "The distribution" <+> pretty dname <+> "has type",
        indent 2 $ pretty fty,
        "But was provided arguments of type",
        indent 2 $ tupled $ pretty <$> given
      ]
  pretty (BadStmt stmt err) =
    vsep
      [ "An error occured in the statement of" <+> pretty stmt,
        indent 2 $ pretty err
      ]
  pretty (BinOpShapeErr binop sh sh') =
    vsep
      [ "In application of" <+> viaShow binop,
        indent 2 $ pretty $ IncompatibleShapes sh sh'
      ]
  pretty (BinOpElTyErr binop e e') =
    vsep
      [ "In application of" <+> viaShow binop,
        "The left hand side has elements of type",
        indent 2 $ pretty e,
        "While the right hand side has elements of type",
        indent 2 $ pretty e'
      ]
  pretty (InvalidGather t1 t2) =
    vsep
      [ "Invalid Gather.",
        "Gather expects as its second argument an array of indices into thee first array",
        "The first argument has type",
        indent 2 $ pretty t1,
        indent 2 $ pretty t2
      ]
  pretty (ExpectedGot t1 t2) =
    vsep
      [ "The compiler was expecting an expression of type",
        indent 2 $ pretty t1,
        "However, it got an expression of type",
        indent 2 $ pretty t2
      ]
  pretty (UnBoundFunctionIdent fname []) =
    "There is no known function" <+> pretty fname
  pretty (UnBoundFunctionIdent fname near) =
    vsep
      [ "There is no known function" <+> pretty fname,
        "Did you mean one of the following?",
        indent 2 . vsep $ ("•" <+>) . pretty <$> near
      ]
  pretty (UnBoundDistIdent fname []) =
    "There is no known distribution" <+> pretty fname
  pretty (UnBoundDistIdent fname near) =
    vsep
      [ "There is no known distribution" <+> pretty fname,
        "Did you mean one of the following?",
        indent 2 . vsep $ ("•" <+>) . pretty <$> near
      ]
  pretty (UnBoundVarIdent fname []) =
    "There is no known variable" <+> pretty fname
  pretty (UnBoundVarIdent fname near) =
    vsep
      [ "There is no known variable" <+> pretty fname,
        "Did you mean one of the following?",
        indent 2 . vsep $ ("•" <+>) . pretty <$> near
      ]
  pretty (NonHomogenousArrayLit tys) = "Nonhomogenous array literal"
  pretty (OtherErr txt) = pretty txt

  pretty (Blame (SourcePos _ line col) err) = vsep 
    [ hsep ["On line",  pretty $ unPos line, "column", pretty $ unPos col] 
    , indent 2 $ pretty err 
    ]
