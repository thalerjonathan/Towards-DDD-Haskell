module Main where

-- import           BDD.Tests
import           Domain.Tests
import           Application.Tests
import           Test.Tasty

main :: IO ()
main = do
  let bankingTests
        = testGroup "Banking Tests"
            [ domain_tests
            , application_tests
            -- , bdd_tests
            ]

  defaultMain bankingTests
