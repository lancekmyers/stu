{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Unification where

import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.QuickCheck
import Tests.Shapes
import Types

check_unify :: TestTree
check_unify =
  testGroup
    "unification"
    [ testProperty "arguments of exactly the right type unify" $ \tys ty ->
        let args = (\t -> ("", t)) <$> tys
         in (unify tys (FunctionTy args ty)) == (Right (Nothing, ty)),
      testProperty "broadcasts over common prefix" $ \prefix tys ty ->
        let tys' = [Ty (prefix <> sh) el pos | (Ty sh el pos) <- tys]
            fTy = FunctionTy ((\t -> ("", t)) <$> tys) ty
            ty' =
              let Ty sh el _pos = ty
               in if length tys > 0
                    then Ty (prefix <> sh) el _pos
                    else Ty sh el _pos
            prefix_ = if (shRank prefix > 0) && (length tys > 0) then (Just prefix) else Nothing
         in (unify tys' fTy) == (Right (prefix_, ty')),
      testProperty "scalar function can take anything" $ \ty ->
        let Ty sh elt _ = ty
            scalarFn = FunctionTy [("x", Ty [] elt Nothing)] (Ty [] elt Nothing)
         in (unify [ty] scalarFn)
              == ( case sh of [] -> Right (Nothing, ty); _ -> (Right (Just sh, ty))
                 ),
      testProperty "vecToScalar" $ \ty card ->
        let Ty sh elt _pos = ty
            ty' = Ty (sh <> [card]) elt _pos
            funTy =
              FunctionTy
                [("x", Ty [CardBV "n"] elt Nothing)]
                (Ty [] elt Nothing)
         in (unify [ty'] funTy)
              == ( case sh of [] -> Right (Nothing, ty); _ -> (Right (Just sh, ty))
                 ),
      testProperty "rejects different length vectors" $ \n ->
        let ty = Ty [CardBV $ T.pack n] REAL Nothing
            ty' = Ty [CardBV $ "n" <> T.pack n] REAL Nothing
         in (broadcastsTo ty ty')
              == ( False
                 )
    ]
