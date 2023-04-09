{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Broadcasting where

import Control.Monad (forM)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.QuickCheck
import Tests.Shapes
import Types

check_broadcastsTo :: TestTree
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
