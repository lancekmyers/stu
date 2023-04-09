{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty.Bench (bgroup, defaultMain)

main =
  defaultMain
    [ bgroup
        "No Benchmarks yet"
        []
    ]
