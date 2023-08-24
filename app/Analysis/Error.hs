{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Analysis.Error where

import AST (BinOp)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Validate (MonadValidate (..), dispute, refute)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Default (def)
import Error.Diagnose
    ( Position(Position),
      addReport,
      Diagnostic,
      Marker(This, Where),
      Note(Note, Hint),
      Report(Err) )
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

type TypeError = Diagnostic T.Text

mkDiagnostic :: Report msg -> Diagnostic msg
mkDiagnostic = addReport def

badFunApp ::
  MonadValidate TypeError m =>
  Text ->
  SrcSpan ->
  [Ty] ->
  FunctionTy ->
  m a
badFunApp fname fnAppPos given fty@(FunctionTy argTys _) =
  refute . mkDiagnostic $
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
  MonadValidate TypeError m =>
  Text ->
  SrcSpan ->
  [Ty] ->
  FunctionTy ->
  m a
badDistr dname fnAppPos given fty@(FunctionTy argTys _) =
  refute . mkDiagnostic $
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
  (MonadValidate TypeError m, Foldable f) =>
  Text ->
  Maybe SrcSpan ->
  Text ->
  f Text ->
  m a
unBoundIdent univ (Just pos) name potential =
  refute . mkDiagnostic $
    Err
      Nothing
      ("Unknown identifier")
      [(pos', This $ "There is no known " <> univ <> " " <> name)]
      (showSuggestions potential)
  where
    Position (l, c) _ fname = pos
    pos' = Position (l, c) (l, c + T.length name) fname
unBoundIdent univ Nothing name potential =
  refute . mkDiagnostic $
    Err
      Nothing
      ("There is no known " <> univ <> " " <> name)
      []
      (showSuggestions potential)

unBoundFunctionIdent ::
  (MonadValidate TypeError m, Foldable f) =>
  Maybe SrcSpan ->
  Text ->
  f Text ->
  m a
unBoundFunctionIdent = unBoundIdent "function"

unBoundDistIdent ::
  (MonadValidate TypeError m, Foldable f) =>
  Maybe SrcSpan ->
  Text ->
  f Text ->
  m a
unBoundDistIdent = unBoundIdent "distribution"

unBoundCardIdent ::
  (MonadValidate TypeError m, Foldable f) =>
  Maybe SrcSpan ->
  Text ->
  f Text ->
  m a
unBoundCardIdent = unBoundIdent "cardinality"

unBoundVarIdent ::
  (MonadValidate TypeError m, Foldable f) =>
  Maybe SrcSpan ->
  Text ->
  f Text ->
  m a
unBoundVarIdent = unBoundIdent "variable"

doesNotMatchDeclaredType ::
  MonadValidate TypeError m =>
  Ty ->
  Ty ->
  m a
doesNotMatchDeclaredType expTy@(Ty _ _ (Just p1)) gotTy@(Ty _ _ (Just p2)) =
  refute . mkDiagnostic $
    Err
      Nothing
      ( "The right hand side does not have the expected type "
          <> (T.pack $ show expTy)
      )
      [ (p2, Where $ "Has type " <> (T.pack $ show gotTy)),
        (p1, Where $ "Was expecting something of this type")
      ]
      []

doesNotMatchReturnType ::
  MonadValidate TypeError m =>
  Ty ->
  Ty ->
  m a
doesNotMatchReturnType expTy@(Ty _ _ (Just p1)) gotTy@(Ty _ _ (Just p2)) =
  refute . mkDiagnostic $
    Err
      Nothing
      ( "The function body does not return the correct type "
          <> (T.pack $ show expTy)
      )
      [ (p2, Where $ "Returns " <> (T.pack $ show gotTy)),
        (p1, Where $ "Was expecting something of this type")
      ]
      []

binOpErr ::
  MonadValidate TypeError m =>
  BinOp ->
  SrcSpan ->
  Ty ->
  Ty ->
  m a
binOpErr op pos t1@(Ty _ _ (Just pos1)) t2@(Ty _ _ (Just pos2)) =
  refute . mkDiagnostic $
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
  refute . mkDiagnostic $
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

invalidFold ::
  MonadValidate TypeError m =>
  SrcSpan ->
  FunctionTy ->
  Ty ->
  Ty ->
  m a
invalidFold loc fty x0@(Ty _ _ (Just x0l)) xs@(Ty _ _ (Just xsl)) =
  refute . mkDiagnostic $
    Err
      Nothing
      "Invalid fold"
      [ (x0l, Where $ "has type " <> (T.pack $ show x0)),
        (xsl, Where $ "has type " <> (T.pack $ show xs)),
        ( let Position (l, c) _ f = loc in Position (l, c) (l, c + 4) f,
          This . T.intercalate "\n" $
            [ "fold expects to be called as fold(f, init, xs) where",
              "  f    : (b : ['m..]real, a : ['n..]real) -> ['m..]real",
              "  init : ['m..]real",
              "  xs   : ['p.., 'n..]real"
            ]
        )
      ]
      []
invalidFold loc fty x0 xs =
  refute . mkDiagnostic $
    Err
      Nothing
      "Invalid fold"
      [ ( let Position (l, c) _ f = loc in Position (l, c) (l, c + 4) f,
          This . T.intercalate "\n" $
            [ "fold expects to be called as fold(f, init, xs) where",
              "  f    : (b : ['m..]real, a : ['n..]real) -> ['m..]real",
              "  init : ['m..]real",
              "  xs   : ['p.., 'n..]real"
            ]
        )
      ]
      [ Note $ "has type " <> (T.pack $ show x0),
        Note $ "has type " <> (T.pack $ show xs)
      ]

invalidScan ::
  MonadValidate TypeError m =>
  SrcSpan ->
  FunctionTy ->
  Ty ->
  Ty ->
  m a
invalidScan loc fty x0@(Ty _ _ (Just x0l)) xs@(Ty _ _ (Just xsl)) =
  refute . mkDiagnostic $
    Err
      Nothing
      "Invalid scan"
      [ (x0l, Where $ "has type " <> (T.pack $ show x0)),
        (xsl, Where $ "has type " <> (T.pack $ show xs)),
        ( let Position (l, c) _ f = loc in Position (l, c) (l, c + 4) f,
          This . T.intercalate "\n" $
            [ "scan expects to be called as scan(f, init, xs) where",
              "  f    : (b : ['m..]real, a : ['n..]real) -> ['m..]real",
              "  init : ['m..]real",
              "  xs   : ['p.., 'n..]real"
            ]
        )
      ]
      []
invalidScan loc fty x0 xs =
  refute . mkDiagnostic $
    Err
      Nothing
      "Invalid scan"
      [ ( let Position (l, c) _ f = loc in Position (l, c) (l, c + 4) f,
          This . T.intercalate "\n" $
            [ "scan expects to be called as scan(f, init, xs) where",
              "  f    : (b : ['m..]real, a : ['n..]real) -> ['m..]real",
              "  init : ['m..]real",
              "  xs   : ['p.., 'n..]real"
            ]
        )
      ]
      [ Note $ "has type " <> (T.pack $ show x0),
        Note $ "has type " <> (T.pack $ show xs)
      ]

nonHomogenousArrayLit :: (MonadValidate TypeError m) => a -> m b
nonHomogenousArrayLit _tys =
  refute . mkDiagnostic $
    Err
      Nothing
      "Nonhomogenous array literal"
      []
      []

otherErr :: MonadValidate TypeError m => Text -> m a
otherErr txt =
  refute . mkDiagnostic $
    Err
      Nothing
      txt
      []
      []
