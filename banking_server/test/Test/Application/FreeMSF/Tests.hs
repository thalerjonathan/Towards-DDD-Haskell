module Test.Application.FreeMSF.Tests
  ( application_freemsf_tests
  ) where

import           Test.Tasty

import           Test.Application.FreeMSF.Deposit
import           Test.Application.FreeMSF.Transfer
import           Test.Application.FreeMSF.Withdraw

application_freemsf_tests :: TestTree
application_freemsf_tests = testGroup "Application FreeMSF Tests"
                              [ deposit_freemsf_tests
                              , withdraw_freemsf_tests
                              , transfer_freemsf_tests
                              ]
