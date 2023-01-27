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
import Text.Megaparsec.Pos (SourcePos (..), unPos)
import Types
  ( Card (CardN),
    ElTy,
    FunctionTy (..),
    Shape (getVec),
    Ty,
    shDiff,
    shRank,
  )

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
prettyError _ (NonHomogenousArrayLit tys) = "Nonhomogenous array literal"
prettyError _ (OtherErr txt) = pretty txt
prettyError src (Blame pos@(SourcePos fname line col) err) =
  vsep
    [ bad "error"
        <+> ( emph . hcat $
                [pretty fname, ":", pretty $ unPos line, ":", pretty $ unPos col]
            ),
      prettySrc src pos,
      indent 2 $ prettyError src err
    ]

prettySrc :: Text -> SourcePos -> Doc AnsiStyle
prettySrc src (SourcePos _ line' col') =
  vsep
    [ indent line_width "|",
      pretty line <+> "|" <+> srcLine,
      (indent line_width "|") <+> pointer
    ]
  where
    line = unPos line'
    col = unPos col'
    line_width = (+ 1) . length . show $ line
    pointer = indent (col - 1) (bad "^")
    srcLine = pretty $ T.lines src !! (line - 1)
