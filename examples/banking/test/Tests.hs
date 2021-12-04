module Main where

import           BDD.Tests
import           Domain.Tests
import           Test.Tasty

main :: IO ()
main = do
  let bankingTests
        = testGroup "Banking Tests"
            [ bdd_tests
            , domain_tests
            ]

  defaultMain bankingTests
