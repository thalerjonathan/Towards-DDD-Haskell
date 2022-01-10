module Test.Domain.Tests
  ( domain_tests
  ) where

import           Test.Tasty

import           Test.Domain.FreeMSF.Account.Tests
import           Test.Domain.Eff.Account.Tests

domain_tests :: TestTree
domain_tests = testGroup "Domain Tests"
                  [ account_freemsf_tests
                  , account_eff_tests
                  ]
