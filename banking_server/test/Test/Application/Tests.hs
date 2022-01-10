module Test.Application.Tests
  ( application_tests
  ) where

import           Test.Tasty

import           Test.Application.Eff.Tests
import           Test.Application.FreeMSF.Tests

application_tests :: TestTree
application_tests = testGroup "Application Tests"
                      [ application_freemsf_tests
                      , application_eff_tests
                      ]
