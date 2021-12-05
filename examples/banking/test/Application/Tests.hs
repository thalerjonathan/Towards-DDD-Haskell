module Application.Tests
  ( application_tests
  ) where

import           Test.Tasty

import           Application.Deposit

application_tests :: TestTree
application_tests = testGroup "Application Tests"
                      [ deposit_tests
                      ]