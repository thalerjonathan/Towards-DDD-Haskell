module Domain.Account.Tests where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Domain.Account.Api
import           Domain.Account.Data
import           Domain.Account.Runner

account_tests :: TestTree
account_tests = testGroup "Account Tests"
                  [ deposit_tests
                  ]

deposit_tests :: TestTree
deposit_tests = testGroup "Deposit Tests"
                  [ given_initial_account_when_balance_then_return_0
                  , given_giro_account_when_multipledeposit_then_reflect_newbalance
                  , given_savings_account_when_deposit_then_exception
                  ]

given_initial_account_when_balance_then_return_0 :: TestTree
given_initial_account_when_balance_then_return_0 = testCase "Initial Account has balance 0" $ do
  -- given
  let a = mkTestAccount Giro
  -- when
  let bal = testAccountProgram (getBalance a)
  -- then
  assertEqual "Balance not as expected" bal 0

given_giro_account_when_multipledeposit_then_reflect_newbalance :: TestTree
given_giro_account_when_multipledeposit_then_reflect_newbalance = testCase "Multiple deposits in Giro increase balance" $ do
  -- given
  let a       = mkTestAccount Giro
      amount1 = 123
      amount2 = 234

  -- when
  let (a', _, _)  = testAccountProgram (deposit a amount1)
  let (a'', _, _) = testAccountProgram (deposit a' amount2)
  let bal         = testAccountProgram (getBalance a)
  let bal'        = testAccountProgram (getBalance a')
  let bal''       = testAccountProgram (getBalance a'')

  -- then
  -- TODO: need to check all return values!
  assertEqual "Balance not as expected" bal 0
  assertEqual "Balance not as expected" bal' amount1
  assertEqual "Balance not as expected" bal'' (amount1 + amount2)

given_savings_account_when_deposit_then_exception :: TestTree
given_savings_account_when_deposit_then_exception = testCase "Deposits in Savings not allowed, not changed balance" $ do
  -- given
  let a       = mkTestAccount Savings
      amount = 123

  -- when
  let (a', ret, _)  = testAccountProgram (deposit a amount)

  let balInit     = testAccountProgram (getBalance a)
  let balAfter    = testAccountProgram (getBalance a')

  -- then
  assertEqual "Balance not as expected" balInit balAfter
  assertBool "Expected Deposit Exception" (isDepositException ret)

isDepositException :: Maybe AccountCommandResult -> Bool
isDepositException (Just (DepositResult (Left _))) = True
isDepositException _ = False