{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Analysis.Error where

import AST (BinOp)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    annotate,
    hcat,
    indent,
    tupled,
    viaShow,
    vsep,
    (<+>),
  )
import Prettyprinter.Render.Terminal
  ( AnsiStyle,
    Color (..),
    bold,
    color,
  )
-- import Text.Megaparsec.Pos (SourcePos (..), unPos)
import Types
  ( Card (CardN),
    ElTy,
    FunctionTy (..),
    Shape (getVec),
    Ty(..),
    shDiff,
    shRank,
  )
import Control.Monad.Except

import Error.Diagnose
import Util (SrcSpan)
import Text.Megaparsec (SourcePos(..), unPos)

incompatibleShapes =  IncompatibleShapes 
badFunApp = BadFunApp 
badDistr = BadDistr
badStmt = BadStmt 
binOpShapeErr = BinOpShapeErr
binOpElTyErr = BinOpElTyErr 
invalidGather = InvalidGather 
expectedGot = ExpectedGot
unBoundFunctionIdent = UnBoundFunctionIdent 
unBoundDistIdent = UnBoundDistIdent
unBoundVarIdent = UnBoundVarIdent
unBoundCardIdent = UnBoundCardIdent
nonHomogenousArrayLit = NonHomogenousArrayLit 
-- blame = Blame 
otherErr = OtherErr 

type TypeError' = Report (Doc AnsiStyle)

badFunApp' 
  :: MonadError  TypeError' m 
  => Text -> SrcSpan -> [(SrcSpan, Ty)] 
  -> FunctionTy 
  -> m a
badFunApp' fname fnAppPos given fty@(FunctionTy argTys _) = throwError $ Err 
  Nothing 
  ("Error in application of" <+> pretty fname) 
  ((fnAppPos, This "Incorrect arguments given") : 
    [ (pos, Where $ vsep
        [ "in the argument" <+> emph (pretty x),
          indent 2 "expected:" <+> (pretty expTy),
          indent 2 "provided:" <+> (pretty gotTy)
        ])
    | ((x, expTy), (pos, gotTy)) <- zip argTys given ])
  [ {- hints -} ]

badDistr' 
  :: MonadError  TypeError' m 
  => Text -> SrcSpan -> [(SrcSpan, Ty)] 
  -> FunctionTy 
  -> m a
badDistr' fname fnAppPos given fty@(FunctionTy argTys _) = throwError $ Err 
  Nothing 
  ("Error in application of distribution" <+> pretty fname) 
  ((fnAppPos, This "Incorrect arguments given") : 
    [ (pos, Where $ vsep
        [ "in the argument" <+> emph (pretty x),
          indent 2 "expected:" <+> (pretty expTy),
          indent 2 "provided:" <+> (pretty gotTy)
        ])
    | ((x, expTy), (pos, gotTy)) <- zip argTys given ])
  [ {- hints -} ]


unBoundIdent 
  :: MonadError  TypeError' m
  => Text -> SrcSpan -> Text -> [Text] -> m a
unBoundIdent univ pos name potential = throwError $ Err 
  Nothing 
  ("Unknown identifier") 
  [(pos, This $ "There is no known" <+> pretty univ <+> pretty name)]
  (Hint "Did you mean ... ?" : [
    Hint . indent 2 . ("•" <+>) $ pretty suggestion
    | suggestion <- potential
  ])

unBoundFunctionIdent' 
  :: MonadError  TypeError' m
  => SrcSpan -> Text -> [Text] -> m a
unBoundFunctionIdent' = unBoundIdent "function"
unBoundDistIdent' 
  :: MonadError  TypeError' m
  => SrcSpan -> Text -> [Text] -> m a
unBoundDistIdent' = unBoundIdent "distribution"
unBoundCardIdent'
  :: MonadError  TypeError' m
  => SrcSpan -> Text -> [Text] -> m a
unBoundCardIdent' = unBoundIdent "cardinality"
unBoundVarIdent'
  :: MonadError  TypeError' m
  => SrcSpan -> Text -> [Text] -> m a
unBoundVarIdent' = unBoundIdent "variable"

doesNotMatchDeclaredType 
  :: MonadError TypeError' m
  => Ty -> Ty -> SrcSpan -> m a
doesNotMatchDeclaredType expTy gotTy pos  = throwError $ Err 
  Nothing 
  ("The right hand side does not have the expected type" <+> pretty expTy)
  [ (pos, Where $ "Has type" <+> pretty gotTy)
  , (pos, Where $ "Was expecting something of this type") ]
  []

binOpErr' 
  :: MonadError TypeError' m 
  => BinOp -> SrcSpan -> Ty -> Ty -> m a
binOpErr' op pos t1@(Ty _ _ (Just pos1)) t2@(Ty _ _ (Just pos2)) 
  = throwError $ Err 
  Nothing 
  ("Cannot apply" <+> (pretty $ show op) <+> "to the given arguments")
  [ (pos, This $ "operation" <+> (pretty $ show op) <+>
      "expects broadcastable arguments")
  , (pos1, Where $ "The left hand side has type" <+> pretty t1)
  , (pos2, Where $ "The right hand side has type" <+> pretty t2)
  ] 
  []
binOpErr' op pos t1 t2 
  = throwError $ Err 
  Nothing 
  ("Cannot apply" <+> (pretty $ show op) <+> "to the given arguments")
  [ (pos, This $ "operation"<+> (pretty $ show op) <+>
      "expects broadcastable arguments")
  ] 
  [ Note $ "The left hand side has type" <+> pretty t1
  , Note $ "The right hand side has type" <+> pretty t2
  ]


nonHomogenousArrayLit' _tys = throwError $ Err
  Nothing 
  "Nonhomogenous array literal"
  [] 
  [] 

otherErr' txt = throwError $ Err 
  Nothing 
  (pretty txt)
  []
  []

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
  | UnBoundCardIdent Card [Text]
  | NonHomogenousArrayLit [Ty]
  | Blame SrcSpan TypeError
  | OtherErr Text
  deriving (Show)

-- blame :: SourcePos -> TypeError -> TypeError
-- blame _ (Blame loc x) = Blame loc x
blame loc x = catchError x $ \case 
  Blame loc err -> throwError $ Blame loc err 
  err -> throwError $ Blame loc err

blameStmt :: MonadError TypeError m => Text -> m a -> m a
blameStmt name x = catchError x $ \err -> throwError (BadStmt name err)

bad :: Doc AnsiStyle -> Doc AnsiStyle
bad = emph . annotate (color Red)

good :: Doc AnsiStyle -> Doc AnsiStyle
good = emph . annotate (color Green)

emph :: Doc AnsiStyle -> Doc AnsiStyle
emph = annotate bold

prettyShapeError :: Shape -> Shape -> (Doc AnsiStyle, Doc AnsiStyle)
prettyShapeError xs ys = (xs', ys')
  where
    prefix :: V.Vector (Doc AnsiStyle)
    prefix = pretty <$> getVec (fromMaybe mempty (shDiff xs ys))
    n = shRank xs `min` shRank ys

    xsPrefix = if (shRank xs > shRank ys) then prefix else []
    ysPrefix = if (shRank ys > shRank xs) then prefix else []

    xsSuffix = V.reverse . V.take n . V.reverse . getVec $ xs
    ysSuffix = V.reverse . V.take n . V.reverse . getVec $ ys

    prettyCards = V.zipWith go xsSuffix ysSuffix

    xs' = tupled . V.toList $ xsPrefix <> (fst <$> prettyCards)
    ys' = tupled . V.toList $ ysPrefix <> (snd <$> prettyCards)

    go x@(CardN 1) y = (good $ pretty x, good $ pretty y)
    go x y@(CardN 1) = (good $ pretty x, good $ pretty y)
    go x y =
      if x == y
        then (good $ pretty x, good $ pretty y)
        else (bad $ pretty x, bad $ pretty y)

prettyError :: T.Text -> TypeError -> Doc AnsiStyle
prettyError _ (IncompatibleShapes sh1 sh2) =
  let (sh1', sh2') = prettyShapeError sh1 sh2
   in vsep
        [ "The shape",
          indent 2 sh1',
          "Does not broadcast with",
          indent 2 sh2'
        ]
prettyError _ (BadFunApp fname given fty@(FunctionTy argTys ret))
  | (length argTys /= length given) =
      vsep
        [ "The function" <+> (bad $ pretty fname) <+> "was applied to the wrong number of arguments",
          (emph $ pretty fname) <+> "expects"
            <+> (emph . pretty . length $ argTys)
            <+> "arguments",
          "but was provided"
            <+> (emph . pretty . length $ given)
            <+> "arguments"
        ]
  | otherwise =
      vsep
        [ "The function" <+> (bad $ pretty fname) <+> "cannot be applied to the given types",
          indent 2 . vsep $ zipWith expGot argTys given
        ]
  where
    expGot (name, expTy) gotTy =
      vsep
        [ "in the argument" <+> emph (pretty name),
          indent 2 "expected:" <+> (pretty expTy),
          indent 2 "provided:" <+> (pretty gotTy)
        ]
prettyError _ (BadDistr dname given fty@(FunctionTy argTys ret))
  | (length argTys /= length given) =
      vsep
        [ "The distribution" <+> (bad $ pretty dname) <+> "was applied to the wrong number of arguments",
          (emph $ pretty dname) <+> "expects"
            <+> (emph . pretty . length $ argTys)
            <+> "arguments",
          "but was prrovided"
            <+> (emph . pretty . length $ given)
            <+> "arguments"
        ]
  | otherwise =
      vsep
        [ "The distribution" <+> (bad $ pretty dname) <+> "cannot be applied to the given types",
          indent 2 . vsep $ zipWith expGot argTys given
        ]
  where
    expGot (name, expTy) gotTy =
      vsep
        [ "in the argument" <+> emph (pretty name),
          indent 2 "expected:" <+> (pretty expTy),
          indent 2 "provided:" <+> (pretty gotTy)
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
prettyError _ (UnBoundCardIdent name []) =
  "There is no known cardinality" <+> (bad $ pretty name)
prettyError _ (UnBoundCardIdent name near) =
  vsep
    [ "There is no known cardinality" <+> (bad $ pretty name),
      "Did you mean one of the following?",
      indent 2 . vsep $ ("•" <+>) . pretty <$> near
    ]
prettyError _ (NonHomogenousArrayLit tys) = "Nonhomogenous array literal"
prettyError _ (OtherErr txt) = pretty txt
prettyError src (Blame pos err) = 
  vsep
    [ bad "error" <+> (emph $ pretty pos)
    , prettySrc src pos
    , indent 2 $ prettyError src err
    ]

prettySrc :: Text -> Position -> Doc AnsiStyle
prettySrc src (Position (l,l') (c, c') fname) =
  vsep
    [ indent line_width "|",
      pretty l <+> "|" <+> srcLine,
      (indent line_width "|") <+> pointer
    ]
  where
    line_width = (+ 1) . length . show $ l
    pointer = indent (c - 1) (bad . pretty $ T.replicate (c' - c) "^")
    srcLine = pretty $ T.lines src !! (l - 1)
