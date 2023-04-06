module Main where

import Broadcasting
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Test Suite"
      [ check_broadcastsTo,
        check_shDiff,
        check_unify
      ]
