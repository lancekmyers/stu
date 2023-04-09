module Main where

import Test.Tasty (defaultMain, testGroup)
import Tests.Broadcasting (check_broadcastsTo)
import Tests.Shapes (cases_shDiff, check_shDiff)
import Tests.Unification (check_unify)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Test Suite"
      [ testGroup
          "Unit Tests"
          [ cases_shDiff
          ],
        testGroup
          "Properties"
          [ check_broadcastsTo,
            check_shDiff,
            check_unify
          ]
      ]
