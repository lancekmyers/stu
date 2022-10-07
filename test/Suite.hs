module Main  where 
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Broadcasting

main :: IO ()
main = hspec $ do
  check_broadcastsTo
  check_shDiff
  check_unify