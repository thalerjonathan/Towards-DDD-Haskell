module Test.Application.Eff.Tests
  ( application_eff_tests
  ) where

import           Test.Tasty

import           Test.Application.Eff.Deposit
import           Test.Application.Eff.Transfer
import           Test.Application.Eff.Withdraw

application_eff_tests :: TestTree
application_eff_tests = testGroup "Application Eff Tests"
                              [ deposit_eff_tests
                              , withdraw_eff_tests
                              , transfer_eff_tests
                              ]
