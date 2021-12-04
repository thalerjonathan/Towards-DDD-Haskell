module Domain.Account.Tests where

import           Test.Tasty
import           Test.Tasty.HUnit

account_tests :: TestTree
account_tests = testGroup "Account Tests"
                  [ deposit_tests
                  ]

deposit_tests :: TestTree
deposit_tests = testCase "Deposit Scenarios" $ do
  assertEqual "Foobar" True True
