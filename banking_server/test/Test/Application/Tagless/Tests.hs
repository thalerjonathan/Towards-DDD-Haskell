module Test.Application.Tagless.Tests
  ( application_tagless_tests
  ) where

import           Test.Tasty

import           Test.Application.Tagless.Deposit
import           Test.Application.Tagless.Transfer
import           Test.Application.Tagless.Withdraw

application_tagless_tests :: TestTree
application_tagless_tests = testGroup "Application Tagless Tests"
                              [ deposit_tagless_tests
                              , withdraw_tagless_tests
                              , transfer_tagless_tests
                              ]
