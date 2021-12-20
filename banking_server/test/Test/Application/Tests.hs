module Test.Application.Tests
  ( application_tests
  ) where

import           Test.Tasty

import           Test.Application.Deposit
import           Test.Application.Transfer
import           Test.Application.Withdraw

application_tests :: TestTree
application_tests = testGroup "Application Tests"
                      [ deposit_tests
                      , withdraw_tests
                      , transfer_tests
                      ]
