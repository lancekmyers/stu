module Main where

import Test.Tasty (defaultMain, testGroup)
import Tests.Broadcasting (check_broadcastsTo)
import Tests.Shapes (check_shDiff)
import Tests.Unification (check_unify)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Test Suite"
      [ check_broadcastsTo,
        check_shDiff,
        check_unify
      ]
