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
import Control.Comonad.Trans.Cofree ( CofreeF((:<)), Cofree, CofreeT, headF, cofree )
-- import qualified Control.Comonad.Cofree as Cofree (Cofree(..))
import Text.Megaparsec.Pos (SourcePos (..), unPos)
import Control.Comonad (extract)
import Control.Comonad.Identity (Identity (runIdentity, Identity))
import Data.Functor.Compose (Compose(getCompose, Compose))
import Prettyprinter.Render.Terminal

data Ctx = Ctx
  { vars :: Map Text Ty,
    funs :: Map Text FunctionTy,
    dists :: Map Text FunctionTy,
    knownCards :: Set Text,
    vardom :: Map Text VarDomain
  } deriving Show

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

annotateVarDomain :: (MonadTyCtx m) => ExprF a -> m (ExprF a)
annotateVarDomain (VarF name _) = do 
  vardoms <- gets vardom
  case M.lookup name vardoms of 
    Nothing -> throwError $ UnBoundVarIdent name []
    Just d  -> return $ VarF name d
annotateVarDomain x = return x

insertTy ::
  forall m.
  (MonadTyCtx m) =>
  Text ->
  VarDomain -> 
  Ty ->
  m ()
insertTy name vd ty = do
  Ctx vars funs dists cards vardom <- get
  let vars' = M.insert name ty vars
  let vardom' = M.insert name vd vardom 
  put $ Ctx vars' funs dists cards vardom'

inferTy :: forall m.
  ( MonadTyCtx m,
    MonadError TypeError m
  ) => Cofree ExprF SourcePos -> m (Cofree ExprF Ty)
inferTy = para (go . runIdentity . getCompose)
  where 
    go :: 
      CofreeF ExprF SourcePos (Cofree ExprF SourcePos, m (Cofree ExprF Ty)) 
      -> m (Cofree ExprF Ty)
    go (loc :< exp) = do 
      let embed' = embed . Compose . Identity
      let foo = fmap (runIdentity . getCompose . project) . snd <$> exp 
      let exp = fmap (\(ty :< exp) -> ty) <$> foo
      ty <- catchError (alg exp) (throwError . blame loc)
      baz <- sequence (fmap (\(ty :< exp) -> exp) <$> foo)
      let qux = ((\e -> embed' $ ty :< e) <$> baz) :: ExprF (Cofree ExprF Ty)
      qux' <- annotateVarDomain qux
      return . embed' $ ty :< qux'

alg :: MonadTyCtx m => ExprF (m Ty) -> m Ty
alg (ArithF binop t1 t2) = do
  ty_lhs@(Ty sh_lhs el_lhs) <- t1
  ty_rhs@(Ty sh_rhs el_rhs) <- t2
  when (el_lhs /= el_rhs) . throwError $ BinOpElTyErr binop el_lhs el_rhs
  case broadcast sh_lhs sh_rhs of
    Nothing -> throwError $ BinOpShapeErr binop sh_lhs sh_rhs
    Just sh -> return $ Ty sh el_lhs
  
alg (VarF name _) = lookupVar name
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
    Right (_, ret) -> return ret
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

cofreeHead :: Functor f => Cofree f a -> a
cofreeHead = headF . runIdentity . getCompose . project
inferTyDist ::
  (MonadTyCtx m) =>
  Distribution SourcePos ->
  m (Distribution Ty) 
inferTyDist (Distribution dname args loc (_, br_sh)) = do
  fty <- lookupDist dname
  -- annotated expressions passed as arguments 
  arg_ann <- traverse inferTy args
  let arg_tys = map cofreeHead arg_ann
  case unify arg_tys fty of
    Left _ -> throwError $ Blame loc $ BadDistr dname arg_tys fty
    Right (bd, ret) -> return $
      Distribution dname arg_ann ret (V.length <$> bd, br_sh)

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
  -- is it ty' - ty or ty - ty'
  let br_sh = shDiff (shape ty') (shape ty)
  let bij' = fmap (const ty') <$> bij
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

blame :: SourcePos -> TypeError -> TypeError 
blame _ (Blame loc x) = Blame loc x 
blame loc err = Blame loc err

bad = annotate (color Red)

prettyError :: TypeError -> Doc AnsiStyle
prettyError (IncompatibleShapes sh sh') =
    vsep
      [ "The shape",
        indent 2 $ prettyShape sh,
        "Does not broadcast with",
        indent 2 $ prettyShape sh'
      ]
prettyError (BadFunApp fname given fty) =
    vsep
      [ "The function" <+> (bad $ pretty fname) <+> "has type",
        indent 2 $ pretty fty,
        "But was provided arguments of type",
        indent 2 $ tupled $ pretty <$> given
      ]
prettyError (BadDistr dname given fty) =
    vsep
      [ "The distribution" <+> (bad $ pretty dname) <+> "has type",
        indent 2 $ pretty fty,
        "But was provided arguments of type",
        indent 2 $ tupled $ pretty <$> given
      ]
prettyError (BadStmt stmt err) =
    vsep
      [ "An error occured in the statement of" <+> pretty stmt,
        indent 2 $ prettyError err
      ]
prettyError (BinOpShapeErr binop sh sh') =
    vsep
      [ "In application of" <+> (bad $ viaShow binop),
        indent 2 $ prettyError $ IncompatibleShapes sh sh'
      ]
prettyError (BinOpElTyErr binop e e') =
    vsep
      [ "In application of" <+> (bad $ viaShow binop),
        "The left hand side has elements of type",
        indent 2 $ annotate bold $ pretty e,
        "While the right hand side has elements of type",
        indent 2 $ annotate bold $ pretty e'
      ]
prettyError (InvalidGather t1 t2) =
    vsep
      [ "Invalid Gather.",
        "Gather expects as its second argument an array of indices into the first array",
        "The first argument has type",
        indent 2 $ pretty t1,
        "The second argument has type",
        indent 2 $ pretty t2
      ]
prettyError (ExpectedGot t1 t2) =
    vsep
      [ "The compiler was expecting an expression of type",
        indent 2 $ pretty t1,
        "However, it got an expression of type",
        indent 2 . bad $ pretty t2
      ]
prettyError (UnBoundFunctionIdent fname []) =
    "There is no known function" <+> (bad $ pretty fname)
prettyError (UnBoundFunctionIdent fname near) =
    vsep
      [ "There is no known function" <+> (bad $ pretty fname),
        "Did you mean one of the following?",
        indent 2 . vsep $ ("•" <+>) . pretty <$> near
      ]
prettyError (UnBoundDistIdent fname []) =
    "There is no known distribution" <+> (bad $ pretty fname)
prettyError (UnBoundDistIdent fname near) =
    vsep
      [ "There is no known distribution" <+> (bad $ pretty fname),
        "Did you mean one of the following?",
        indent 2 . vsep $ ("•" <+>) . pretty <$> near
      ]
prettyError (UnBoundVarIdent fname []) =
    "There is no known variable" <+> (bad $ pretty fname)
prettyError (UnBoundVarIdent fname near) =
    vsep
      [ "There is no known variable" <+> (bad $ pretty fname),
        "Did you mean one of the following?",
        indent 2 . vsep $ ("•" <+>) . pretty <$> near
      ]
prettyError (NonHomogenousArrayLit tys) = "Nonhomogenous array literal"
prettyError (OtherErr txt) = pretty txt

prettyError (Blame (SourcePos _ line col) err) = vsep 
    [ hsep ["On line",  pretty $ unPos line, "column", pretty $ unPos col] 
    , indent 2 $ prettyError err 
    ]
