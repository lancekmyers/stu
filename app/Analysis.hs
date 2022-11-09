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
import qualified Data.Text as T
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
import Data.Maybe (fromMaybe, mapMaybe)

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

buildCtx :: [Decl] -> Ctx
buildCtx decls = Ctx vars funs dists knownCards varDoms
  where
    vars :: Map Text Ty
    vars = M.fromList $ mapMaybe go decls
      where
        go (DataDecl name ty) = Just (name, ty)
        go _ = Nothing
    varDoms = M.fromList $ mapMaybe go decls
      where
        go (DataDecl name ty) = Just (name, Data)
        go _ = Nothing

    scalarFunTy = FunctionTy 0 [("x", Ty [] REAL)] (Ty [] REAL)
    funs :: Map Text FunctionTy
    funs =
      M.fromList
        [ ("sin",  scalarFunTy),
          ("cos",  scalarFunTy),
          ("tan",  scalarFunTy),
          ("exp",  scalarFunTy),
          ("ln",   scalarFunTy),
          ("sqrt", scalarFunTy),
          ("logit", scalarFunTy), 
          ("mean", FunctionTy 1 [("x", Ty [CardBV 0] REAL)] real)
        ]
    real = Ty [] REAL
    locScale = FunctionTy 0 [("loc", real), ("scale", real)] real
    dists :: Map Text FunctionTy
    dists =
      M.fromList
        [ ("Normal", locScale),
          ("HalfNormal", locScale),
          ("Bernoulli", FunctionTy 0 [("prob", real)] (Ty [] INT)),
          ("Binomial", FunctionTy 0 [("n", Ty [] INT), ("prob", real)] 
            (Ty [] INT)),
          ("Poisson", FunctionTy 0 [("mu", real)] (Ty [] INT)),
          ("Beta", FunctionTy 0 [("alpha", real), ("beta", real)] (real)),
          ("Gamma", 
            FunctionTy 0 [("concentration", real), ("rate", real)] (real)),
          ("MVNormal", FunctionTy 1 
            [ ("mu", Ty [CardBV 0] REAL)
            , ("sigma", Ty [CardBV 0, CardBV 0] REAL)
            ]
            real),
          ("MVNormal", FunctionTy 1 
            [ ("mu", Ty [CardBV 0] REAL)
            , ("sigma", Ty [CardBV 0] REAL)
            ] 
            (Ty [CardBV 0] REAL))
        ]

    knownCards :: Set Text
    knownCards = S.fromList $ mapMaybe go decls
      where
        go (CardDecl name) = Just name
        go (FactorDecl name) = Just name
        go _ = Nothing




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

bad :: Doc AnsiStyle -> Doc AnsiStyle
bad = emph . annotate (color Red)
good :: Doc AnsiStyle -> Doc AnsiStyle
good =  emph . annotate (color Green)
emph :: Doc AnsiStyle -> Doc AnsiStyle
emph = annotate bold

prettyShapeError :: V.Vector Card -> V.Vector Card -> (Doc AnsiStyle, Doc AnsiStyle)
prettyShapeError xs ys = (xs', ys') 
  where 
    prefix :: V.Vector (Doc AnsiStyle)
    prefix = pretty <$> fromMaybe [] (shDiff xs ys)
    n = V.length xs `min` V.length ys 
    
    xsPrefix = if (V.length xs > V.length ys) then prefix else []
    ysPrefix = if (V.length ys > V.length xs) then prefix else []

    xsSuffix = V.reverse . V.take n . V.reverse $ xs
    ysSuffix = V.reverse . V.take n . V.reverse $ ys

    prettyCards = V.zipWith go xsSuffix ysSuffix 

    xs' = tupled . V.toList $ xsPrefix <> (fst <$> prettyCards)
    ys' = tupled . V.toList $ ysPrefix <> (snd <$> prettyCards)

    go x@(CardN 1) y = (good $ pretty x, good $ pretty y) 
    go x y@(CardN 1) = (good $ pretty x, good $ pretty y) 
    go x y = if x == y 
      then (good $ pretty x, good $ pretty y) 
      else (bad  $ pretty x, bad  $ pretty y) 



prettyError :: T.Text -> TypeError -> Doc AnsiStyle
prettyError _ (IncompatibleShapes sh1 sh2) =
  let (sh1', sh2') = prettyShapeError sh1 sh2 
  in vsep
      [ "The shape",
        indent 2 sh1',
        "Does not broadcast with",
        indent 2 sh2'
      ]
prettyError _ (BadFunApp fname given fty@(FunctionTy _ argTys ret)) 
  | (length argTys /= length given) = vsep 
    [ "The function" <+> (bad $ pretty fname) <+> "was applied to the wrong number of arguments"
    , (emph $ pretty fname) <+> "expects" 
      <+> (emph . pretty . length $ argTys) <+> "arguments"
    , "but was provided" 
      <+> (emph . pretty . length $ given) <+> "arguments"
    ]
  | otherwise = vsep
    [ "The function" <+> (bad $ pretty fname) <+> "cannot be applied to the given types",
    indent 2 . vsep $ zipWith expGot argTys given
    ]
  where 
    expGot (name, expTy) gotTy = vsep 
      [ "in the argument" <+> emph (pretty name)
      , indent 2 "expected:" <+> (pretty expTy)
      , indent 2 "provided:" <+> (pretty gotTy)
      ]
prettyError _ (BadDistr dname given fty@(FunctionTy _ argTys ret)) 
  | (length argTys /= length given) = vsep 
    [ "The distribution" <+> (bad $ pretty dname) <+> "was applied to the wrong number of arguments"
    , (emph $ pretty dname) <+> "expects" 
      <+> (emph . pretty . length $ argTys) <+> "arguments"
    , "but was prrovided" 
      <+> (emph . pretty . length $ given) <+> "arguments"
    ]
  | otherwise = vsep
    [ "The distribution" <+> (bad $ pretty dname) <+> "cannot be applied to the given types",
    indent 2 . vsep $ zipWith expGot argTys given
    ]
  where 
    expGot (name, expTy) gotTy = vsep 
      [ "in the argument" <+> emph (pretty name)
      , indent 2 "expected:" <+> (pretty expTy)
      , indent 2 "provided:" <+> (pretty gotTy)
      ]
prettyError src (BadStmt stmt err) =
    vsep
      [ "An error occured in the statement of" <+> pretty stmt,
        prettyError src err
      ]
prettyError _ (BinOpShapeErr binop sh sh') =
    vsep
      [ "In application of" <+> (bad $ viaShow binop),
        indent 2 $ prettyError mempty $ IncompatibleShapes sh sh'
      ]
prettyError _ (BinOpElTyErr binop e e') =
    vsep
      [ "In application of" <+> (bad $ viaShow binop),
        "The left hand side has elements of type",
        indent 2 $ annotate bold $ pretty e,
        "While the right hand side has elements of type",
        indent 2 $ annotate bold $ pretty e'
      ]
prettyError _ (InvalidGather t1 t2) =
    vsep
      [ "Invalid Gather.",
        "Gather expects as its second argument an array of indices into the first array",
        "The first argument has type",
        indent 2 $ pretty t1,
        "The second argument has type",
        indent 2 $ pretty t2
      ]
prettyError _ (ExpectedGot t1 t2) =
    vsep
      [ "The compiler was expecting an expression of type",
        indent 2 $ pretty t1,
        "However, it got an expression of type",
        indent 2 . bad $ pretty t2
      ]
prettyError _ (UnBoundFunctionIdent fname []) =
    "There is no known function" <+> (bad $ pretty fname)
prettyError _ (UnBoundFunctionIdent fname near) =
    vsep
      [ "There is no known function" <+> (bad $ pretty fname),
        "Did you mean one of the following?",
        indent 2 . vsep $ ("•" <+>) . pretty <$> near
      ]
prettyError _ (UnBoundDistIdent fname []) =
    "There is no known distribution" <+> (bad $ pretty fname)
prettyError _ (UnBoundDistIdent fname near) =
    vsep
      [ "There is no known distribution" <+> (bad $ pretty fname),
        "Did you mean one of the following?",
        indent 2 . vsep $ ("•" <+>) . pretty <$> near
      ]
prettyError _ (UnBoundVarIdent fname []) =
    "There is no known variable" <+> (bad $ pretty fname)
prettyError _ (UnBoundVarIdent fname near) =
    vsep
      [ "There is no known variable" <+> (bad $ pretty fname),
        "Did you mean one of the following?",
        indent 2 . vsep $ ("•" <+>) . pretty <$> near
      ]
prettyError _ (NonHomogenousArrayLit tys) = "Nonhomogenous array literal"
prettyError _ (OtherErr txt) = pretty txt

prettyError src (Blame pos@(SourcePos fname line col) err) = vsep 
    [ bad "error" <+> (emph . hcat $
      [pretty fname, ":" , pretty $ unPos line, ":", pretty $ unPos col] 
      )
    , prettySrc src pos
    , indent 2 $ prettyError src err 
    ]


prettySrc :: Text -> SourcePos -> Doc AnsiStyle 
prettySrc src (SourcePos _ line' col') = vsep 
  [ indent line_width "|"
  , pretty line <+> "|" <+> srcLine 
  , (indent line_width "|") <+> pointer
  ]  
  where 
    line = unPos line'
    col = unPos col'
    line_width = (+1) . length . show  $ line
    pointer = indent (col - 1) (bad "^")
    srcLine = pretty $ T.lines src !! (line - 1)