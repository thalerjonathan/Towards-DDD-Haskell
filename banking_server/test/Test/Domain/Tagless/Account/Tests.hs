module Test.Domain.Tagless.Account.Tests where

import           Control.Monad.Except
import qualified Data.Text                   as T
import           Domain.Tagless.Account
import           Domain.Types
import           Test.Application.Tagless.Runner
import           Test.Domain.Tagless.Account.Data
import           Test.Domain.Utils
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils.Time

account_tagless_tests :: TestTree
account_tagless_tests = testGroup "Account Tagless Tests"
                  [ deposit_tests
                  ]

deposit_tests :: TestTree
deposit_tests = testGroup "Deposit Tests"
                  [ given_giro_account_when_multipledeposit_then_reflect_newbalance
                  , given_giro_account_when_depositandwithdraw_then_reflect_newbalance
                  , given_giro_account_when_overdrawlimit_then_exception
                  , given_giro_account_when_overdrawlimit_fromtransfer_then_exception
                  , given_savings_account_when_overdrawlimit_fromtransfer_then_exception
                  , given_savings_account_when_deposit_then_exception
                  , given_savings_account_when_withdraw_then_exception
                  ]

given_giro_account_when_multipledeposit_then_reflect_newbalance :: TestTree
given_giro_account_when_multipledeposit_then_reflect_newbalance = testCase "Multiple deposits in Giro increase balance" $ do
  -- given
  let a       = mkTestAccount Giro
      amount1 = 123
      amount2 = 234
      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [] []

  -- when
  let (_tx, a')   = unsafeFromRight $ testApplication appData (runExceptT $ accountDeposit a amount1)
  let (_tx', a'') = unsafeFromRight $ testApplication appData (runExceptT $ accountDeposit a' amount2)

  let bal         = _aBalance a
  let bal'        = _aBalance a'
  let bal''       = _aBalance a''

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
      now            = mkUTCTime 2021 03 01 0 0 0
      appData        = mkPureAppData now [] []

  -- when
  let (_tx, a')   = unsafeFromRight $ testApplication appData (runExceptT $ accountDeposit a depositAmount)
  let (_tx', a'') = unsafeFromRight $ testApplication appData (runExceptT $ accountWithdraw a' withdrawAmount)

  let bal         = _aBalance a
  let bal'        = _aBalance a'
  let bal''       = _aBalance a''

  -- then
  assertEqual "Balance not as expected" bal 0
  assertEqual "Balance not as expected" bal' depositAmount
  assertEqual "Balance not as expected" bal'' (depositAmount - withdrawAmount)


given_giro_account_when_overdrawlimit_then_exception :: TestTree
given_giro_account_when_overdrawlimit_then_exception = testCase "Cannot overdraw Giro account by more than -1000" $ do
  -- given
  let a       = mkTestAccount Giro
      amount  = 1001
      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [] []

  -- when
  let ret = testApplication appData (runExceptT $ accountWithdraw a amount)

  -- then
  expectException ret "Cannot overdraw Giro account by more than -1000!"


given_giro_account_when_overdrawlimit_fromtransfer_then_exception :: TestTree
given_giro_account_when_overdrawlimit_fromtransfer_then_exception = testCase "Cannot overdraw Giro account through transferring by more than -1000" $ do
  -- given
  let a       = mkTestAccount Giro
      i       = Iban "AT 1234"
      name    = "Test Name"
      ref     = "Test Reference"
      amount  = 1001
      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [] []

  -- when
  let ret = testApplication appData (runExceptT $ accountTransferTo a i amount name ref)

  -- then
  expectException ret "Cannot overdraw Giro account by more than -1000!"

given_savings_account_when_overdrawlimit_fromtransfer_then_exception :: TestTree
given_savings_account_when_overdrawlimit_fromtransfer_then_exception = testCase "Cannot overdraw Savings account through transferring " $ do
  -- given
  let a       = mkTestAccount Savings
      i       = Iban "AT 1234"
      name    = "Test Name"
      ref     = "Test Reference"
      amount  = 1
      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [] []

  -- when
  let ret = testApplication appData (runExceptT $ accountTransferTo a i amount name ref)

  -- then
  expectException ret "Cannot overdraw Savings account!"

given_savings_account_when_deposit_then_exception :: TestTree
given_savings_account_when_deposit_then_exception = testCase "Deposits in Savings not allowed, not changed balance" $ do
  -- given
  let a       = mkTestAccount Savings
      amount  = 123
      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [] []

  -- when
  let ret = testApplication appData (runExceptT $ accountDeposit a amount)

  -- then
  expectException ret "Cannot deposit money directly into Savings Account! Use transfer of money from a Giro Account of the same customer."


given_savings_account_when_withdraw_then_exception :: TestTree
given_savings_account_when_withdraw_then_exception = testCase "Withdrawings from Savings not allowed, not changed balance" $ do
  -- given
  let a       = mkTestAccount Savings
      amount  = 123
      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [] []
      
  -- when
  let ret = testApplication appData (runExceptT $ accountWithdraw a amount)

  -- then
  expectException ret "Cannot withdraw money directly from Savings Account! Use transfer of money into a Giro Account of the same customer."

expectException :: Either T.Text r -> T.Text -> Assertion
expectException (Left err) expectedErr = assertEqual "Exception not matching" err expectedErr
expectException _ expectedErr = assertFailure $ "Expected Exception: '" ++ T.unpack expectedErr ++ "'"
