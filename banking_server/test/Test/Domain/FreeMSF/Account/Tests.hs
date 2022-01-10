module Test.Domain.FreeMSF.Account.Tests where

import           Data.Text
import           Domain.FreeMSF.Account.Api
import           Domain.Types
import           Test.Domain.FreeMSF.Account.Data
import           Test.Domain.FreeMSF.Account.Runner
import           Test.Tasty
import           Test.Tasty.HUnit

account_freemsf_tests :: TestTree
account_freemsf_tests = testGroup "Account FreeMSF Tests"
                  [ deposit_tests
                  ]

deposit_tests :: TestTree
deposit_tests = testGroup "Deposit Tests"
                  [ given_initial_account_when_balance_then_return_0
                  , given_giro_account_when_multipledeposit_then_reflect_newbalance
                  , given_giro_account_when_depositandwithdraw_then_reflect_newbalance
                  , given_giro_account_when_overdrawlimit_then_exception
                  , given_giro_account_when_overdrawlimit_fromtransfer_then_exception
                  , given_savings_account_when_overdrawlimit_fromtransfer_then_exception
                  , given_savings_account_when_deposit_then_exception
                  , given_savings_account_when_withdraw_then_exception
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
  assertEqual "Balance not as expected" bal 0
  assertEqual "Balance not as expected" bal' amount1
  assertEqual "Balance not as expected" bal'' (amount1 + amount2)

given_giro_account_when_depositandwithdraw_then_reflect_newbalance :: TestTree
given_giro_account_when_depositandwithdraw_then_reflect_newbalance = testCase "Deposit and withdraw in Giro change balance" $ do
  -- given
  let a              = mkTestAccount Giro
      depositAmount  = 123
      withdrawAmount = 234

  -- when
  let (a', _, _)  = testAccountProgram (deposit a depositAmount)
  let (a'', _, _) = testAccountProgram (withdraw a' withdrawAmount)
  let bal         = testAccountProgram (getBalance a)
  let bal'        = testAccountProgram (getBalance a')
  let bal''       = testAccountProgram (getBalance a'')

  -- then
  assertEqual "Balance not as expected" bal 0
  assertEqual "Balance not as expected" bal' depositAmount
  assertEqual "Balance not as expected" bal'' (depositAmount - withdrawAmount)


given_giro_account_when_overdrawlimit_then_exception :: TestTree
given_giro_account_when_overdrawlimit_then_exception = testCase "Cannot overdraw Giro account by more than -1000" $ do
  -- given
  let a       = mkTestAccount Giro
      amount = 1001

  -- when
  let (a', ret, _)  = testAccountProgram (withdraw a amount)

  let balInit     = testAccountProgram (getBalance a)
  let balAfter    = testAccountProgram (getBalance a')

  -- then
  assertEqual "Balance not as expected" balInit balAfter
  expectWithdrawException ret "Cannot overdraw Giro account by more than -1000!"

given_giro_account_when_overdrawlimit_fromtransfer_then_exception :: TestTree
given_giro_account_when_overdrawlimit_fromtransfer_then_exception = testCase "Cannot overdraw Giro account through transferring by more than -1000" $ do
  -- given
  let a       = mkTestAccount Giro
      i       = Iban "AT 1234"
      name    = "Test Name"
      ref     = "Test Reference"
      amount  = 1001

  -- when
  let (a', ret, _)  = testAccountProgram (transferTo a i amount name ref)

  let balInit     = testAccountProgram (getBalance a)
  let balAfter    = testAccountProgram (getBalance a')

  -- then
  assertEqual "Balance not as expected" balInit balAfter
  expectTransferToException ret "Cannot overdraw Giro account by more than -1000!"

given_savings_account_when_overdrawlimit_fromtransfer_then_exception :: TestTree
given_savings_account_when_overdrawlimit_fromtransfer_then_exception = testCase "Cannot overdraw Savings account through transferring " $ do
  -- given
  let a       = mkTestAccount Savings
      i       = Iban "AT 1234"
      name    = "Test Name"
      ref     = "Test Reference"
      amount  = 1

  -- when
  let (a', ret, _)  = testAccountProgram (transferTo a i amount name ref)

  let balInit     = testAccountProgram (getBalance a)
  let balAfter    = testAccountProgram (getBalance a')

  -- then
  assertEqual "Balance not as expected" balInit balAfter
  expectTransferToException ret "Cannot overdraw Savings account!"


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
  expectDepositException ret "Cannot deposit money directly into Savings Account! Use transfer of money from a Giro Account of the same customer."


given_savings_account_when_withdraw_then_exception :: TestTree
given_savings_account_when_withdraw_then_exception = testCase "Withdrawings from Savings not allowed, not changed balance" $ do
  -- given
  let a       = mkTestAccount Savings
      amount = 123

  -- when
  let (a', ret, _)  = testAccountProgram (withdraw a amount)

  let balInit     = testAccountProgram (getBalance a)
  let balAfter    = testAccountProgram (getBalance a')

  -- then
  assertEqual "Balance not as expected" balInit balAfter
  expectWithdrawException ret "Cannot withdraw money directly from Savings Account! Use transfer of money into a Giro Account of the same customer."

expectDepositException :: AccountCommandResult -> Text -> Assertion
expectDepositException (DepositResult (Left err)) expectedErr = assertEqual "Deposit Exception not matching" err expectedErr
expectDepositException _ _ = assertFailure "Expected DepositResult Exception"

expectWithdrawException :: AccountCommandResult -> Text -> Assertion
expectWithdrawException (WithdrawResult (Left err)) expectedErr = assertEqual "Withdraw Exception not matching" err expectedErr
expectWithdrawException _ _ = assertFailure "Expected Withdraw Exception"

expectTransferToException :: AccountCommandResult -> Text -> Assertion
expectTransferToException (TransferToResult (Left err)) expectedErr = assertEqual "Transfer Exception not matching" err expectedErr
expectTransferToException _ _ = assertFailure "Expected Transfer Exception"
