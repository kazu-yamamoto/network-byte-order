module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "-XOverloadedStrings"
  , "-XCPP"
  , "Network/ByteOrder.hs"
  ]
