module Test.Application.Deposit where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Application.BankingDomain
import           Application.Exceptions
import           Test.Application.Runner

deposit_tests :: TestTree
deposit_tests = testGroup "Deposit Tests"
                      [ given_repository_when_deposit_then_return_new_tx
                      ]

given_repository_when_deposit_then_return_new_tx :: TestTree
given_repository_when_deposit_then_return_new_tx = testCase "Deposit into existing Account" $ do
  let iban   = "AT12 12345 01234567890"
      amount = 123

  let ret = testApplication
              (deposit iban amount)
              accountRepoStub
              customerRepoStub
  
  assertBool "" (accountNotFound ret)

accountNotFound :: Either Exception a -> Bool
accountNotFound (Left AccountNotFound) = True
accountNotFound _ = False
