{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Analysis.Error where

import AST (BinOp)
import Control.Monad.Except ( MonadError(throwError) )
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Error.Diagnose hiding (pretty)
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
import Text.Megaparsec (SourcePos (..), unPos)
import Types
  ( Card (CardN),
    ElTy,
    FunctionTy (..),
    Shape (getVec),
    Ty (..),
    shDiff,
    shRank,
  )
import Util (SrcSpan)

type TypeError = Report T.Text

badFunApp ::
  MonadError TypeError m =>
  Text ->
  SrcSpan ->
  [Ty] ->
  FunctionTy ->
  m a
badFunApp fname fnAppPos given fty@(FunctionTy argTys _) =
  throwError $
    Err
      Nothing
      ("Error in application of function " <> fname)
      ( (fnAppPos, This "Incorrect arguments given") :
          [ ( pos,
              Where $
                T.unlines
                  [ "in the argument " <> x,
                    "  expected: " <> (T.pack $ show expTy),
                    "  provided: " <> (T.pack $ show gotTy)
                  ]
            )
            | ((x, expTy), gotTy@(Ty _ _ (Just pos))) <- zip argTys given
          ]
      )
      [ Note $
          "The function " <> fname <> " expects to be called like\n"
            <> fname
            <> (T.pack $ show fty)
      ]

badDistr ::
  MonadError TypeError m =>
  Text ->
  SrcSpan ->
  [Ty] ->
  FunctionTy ->
  m a
badDistr dname fnAppPos given fty@(FunctionTy argTys _) =
  throwError $
    Err
      Nothing
      ("Error in application of distribution " <> dname)
      ( (fnAppPos, This "Incorrect arguments given") :
          [ ( pos,
              Where $
                T.unlines
                  [ "in the argument " <> x,
                    "  expected: " <> (T.pack $ show expTy),
                    "  provided: " <> (T.pack $ show gotTy)
                  ]
            )
            | ((x, expTy), gotTy@(Ty _ _ (Just pos))) <- zip argTys given
          ]
      )
      [ Note $
          "The distribution " <> dname <> " expects arrguments the form\n"
            <> dname
            <> (T.pack $ show fty)
      ]

showSuggestions suggs =
  [ Hint $
      "Did you mean one of the following?\n"
        <> T.unlines ["  - " <> sugg | sugg <- take 4 $ toList suggs]
  ]

unBoundIdent ::
  (MonadError TypeError m, Foldable f) =>
  Text ->
  Maybe SrcSpan ->
  Text ->
  f Text ->
  m a
unBoundIdent univ (Just pos) name potential =
  throwError $
    Err
      Nothing
      ("Unknown identifier")
      [(pos', This $ "There is no known " <> univ <> " " <> name)]
      (showSuggestions potential)
  where
    Position (l, c) _ fname = pos
    pos' = Position (l, c) (l, c + T.length name) fname
unBoundIdent univ Nothing name potential =
  throwError $
    Err
      Nothing
      ("There is no known " <> univ <> " " <> name)
      []
      (showSuggestions potential)

unBoundFunctionIdent ::
  (MonadError TypeError m, Foldable f) =>
  Maybe SrcSpan ->
  Text ->
  f Text ->
  m a
unBoundFunctionIdent = unBoundIdent "function"

unBoundDistIdent ::
  (MonadError TypeError m, Foldable f) =>
  Maybe SrcSpan ->
  Text ->
  f Text ->
  m a
unBoundDistIdent = unBoundIdent "distribution"

unBoundCardIdent ::
  (MonadError TypeError m, Foldable f) =>
  Maybe SrcSpan ->
  Text ->
  f Text ->
  m a
unBoundCardIdent = unBoundIdent "cardinality"

unBoundVarIdent ::
  (MonadError TypeError m, Foldable f) =>
  Maybe SrcSpan ->
  Text ->
  f Text ->
  m a
unBoundVarIdent = unBoundIdent "variable"

doesNotMatchDeclaredType ::
  MonadError TypeError m =>
  Ty ->
  Ty ->
  m a
doesNotMatchDeclaredType expTy@(Ty _ _ (Just p1)) gotTy@(Ty _ _ (Just p2)) =
  throwError $
    Err
      Nothing
      ( "The right hand side does not have the expected type"
          <> (T.pack $ show expTy)
      )
      [ (p2, Where $ "Has type " <> (T.pack $ show gotTy)),
        (p1, Where $ "Was expecting something of this type")
      ]
      []

doesNotMatchReturnType ::
  MonadError TypeError m =>
  Ty ->
  Ty ->
  m a
doesNotMatchReturnType expTy@(Ty _ _ (Just p1)) gotTy@(Ty _ _ (Just p2)) =
  throwError $
    Err
      Nothing
      ( "The function body does not return the correct type"
          <> (T.pack $ show expTy)
      )
      [ (p2, Where $ "Returns " <> (T.pack $ show gotTy)),
        (p1, Where $ "Was expecting something of this type")
      ]
      []

binOpErr ::
  MonadError TypeError m =>
  BinOp ->
  SrcSpan ->
  Ty ->
  Ty ->
  m a
binOpErr op pos t1@(Ty _ _ (Just pos1)) t2@(Ty _ _ (Just pos2)) =
  throwError $
    Err
      Nothing
      ("Cannot apply " <> (T.pack $ show op) <> " to the given arguments")
      [ ( pos,
          This $
            "operation " <> (T.pack $ show op)
              <> " expects broadcastable arguments"
        ),
        (pos1, Where $ "The left hand side has type " <> (T.pack $ show t1)),
        (pos2, Where $ "The right hand side has type " <> (T.pack $ show t2))
      ]
      []
binOpErr op pos t1 t2 =
  throwError $
    Err
      Nothing
      ("Cannot apply " <> (T.pack $ show op) <> " to the given arguments")
      [ ( pos,
          This $
            "operation " <> (T.pack $ show op)
              <> " expects broadcastable arguments"
        )
      ]
      [ Note $ "The left hand side has type " <> (T.pack $ show t1),
        Note $ "The right hand side has type " <> (T.pack $ show t2)
      ]

invalidGather ::
  MonadError TypeError m =>
  SrcSpan ->
  Ty ->
  Ty ->
  m a
invalidGather loc t1 t2 =
  throwError $
    Err
      Nothing
      "Invalid gather"
      [(loc, Blank)]
      []

nonHomogenousArrayLit _tys =
  throwError $
    Err
      Nothing
      "Nonhomogenous array literal"
      []
      []

otherErr :: MonadError TypeError m => Text -> m a
otherErr txt =
  throwError $
    Err
      Nothing
      txt
      []
      []
