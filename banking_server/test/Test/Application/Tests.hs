module Test.Application.Tests
  ( application_tests
  ) where

import           Test.Tasty

import           Test.Application.Deposit

application_tests :: TestTree
application_tests = testGroup "Application Tests"
                      [ deposit_tests
                      ]