{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Broadcasting where

-- import Test.Hspec

-- import Test.QuickCheck
-- import Control.Exception (evaluate)

import Control.Monad (forM)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.QuickCheck
import Types

instance Arbitrary Shape where
  arbitrary = MkShape . V.fromList <$> listOf (arbitrary :: Gen Card)

instance Arbitrary Card where
  arbitrary = oneof [n, v]
    where
      n = CardN <$> arbitrarySizedNatural
      v = CardFV . T.pack <$> listOf1 (elements ['A' .. 'Z'])

instance Arbitrary Ty where
  arbitrary = do
    sh <- arbitrary
    el <- elements [REAL, INT]
    return $ Ty sh el Nothing

-- check_broadcastsTo :: SpecWith ()
check_broadcastsTo =
  testGroup
    "shape broadcasts to : "
    [ testProperty "shape with itself" $
        \sh -> sh `shapeBroadcastsTo` sh,
      testProperty "empty to all" $
        \sh -> [] `shapeBroadcastsTo` sh,
      testProperty "nonempy fails to empty " $
        \x sh ->
          let sh' = shCons x sh
           in not $ sh' `shapeBroadcastsTo` [],
      testProperty "axes with 1 broadcast up arbitrarily" $
        \sh_mask ->
          let (sh, mask) = (fst <$> sh_mask, snd <$> sh_mask)
              make_1s mask = zipWith (\b x -> if b then x else CardN 1) mask
              sh_1 = make_1s mask sh
           in (shFromList sh_1) `shapeBroadcastsTo` (shFromList sh),
      testProperty "shape to itself plus a prefix" $
        \sh prefix ->
          let sh_rhs = prefix <> sh
              sh_lhs = sh
           in sh_lhs `shapeBroadcastsTo` sh_rhs
    ]

-- check_shDiff :: SpecWith ()
check_shDiff =
  testGroup
    "shape difference: "
    [ testProperty "shape minus nothing" $
        \sh ->
          shDiff sh [] == (if shRank sh == 0 then Nothing else Just sh),
      testProperty "shape minus itself" $
        \sh ->
          sh `shDiff` sh == Nothing,
      testProperty "prefix + shape - shape = prefix" $
        \prefix sh ->
          (prefix <> sh) `shDiff` sh
            == (if shRank prefix == 0 then Nothing else Just prefix)
    ]

-- check_unify :: SpecWith _
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

-- check to see if vector functions can be applied to scalars
-- sum(1.0) should fail
