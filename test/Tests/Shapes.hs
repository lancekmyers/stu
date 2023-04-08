module Tests.Shapes where

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

check_shDiff :: TestTree
check_shDiff =
  testGroup
    "shape difference: "
    [ testProperty "shape minus nothing" $
        \sh ->
          shDiff sh mempty == (if shRank sh == 0 then Nothing else Just sh),
      testProperty "shape minus itself" $
        \sh ->
          sh `shDiff` sh == Nothing,
      testProperty "prefix + shape - shape = prefix" $
        \prefix sh ->
          (prefix <> sh) `shDiff` sh
            == (if shRank prefix == 0 then Nothing else Just prefix)
    ]
