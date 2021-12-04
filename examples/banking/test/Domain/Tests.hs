module Domain.Tests
  ( domain_tests
  ) where

import           Test.Tasty

import           Domain.Account.Tests

domain_tests :: TestTree
domain_tests = testGroup "Domain Tests"
                  [ account_tests
                  ]
