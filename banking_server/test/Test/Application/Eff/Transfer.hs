module Test.Application.Eff.Transfer where

import           Application.DTO
import           Application.Eff.Banking     as Banking
import           Control.Monad.Except
import           Data.UUID
import           Data.UUID.V4                (nextRandom)
import           Domain.Eff.Account
import           Domain.Eff.Customer
import           Domain.Types
import           Test.Application.Eff.Runner
import           Test.Application.Utils
import           Test.Domain.Eff.Account.Data
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils.Time

transfer_eff_tests :: TestTree
transfer_eff_tests = testGroup "Transfer Eff Tests"
                      [ given_missingfromaccount_when_transfer_then_exception
                      , given_missingtoaccount_when_transfer_then_exception
                      , given_fromgirotosavingsaccount_when_transfer_then_exception
                      , given_fromsavingstogiroaccount_when_transfer_then_exception
                      , given_amountLT5000_when_transfer_then_exception
                      , given_missingfromCustomer_when_transfer_then_exception
                      , given_missingtoCustomer_when_transfer_then_exception
                      , given_accountsdifferentcustomers_when_transfer_then_returnTxLine
                      , given_accountssamecustomer_when_transfer_then_returnTxLine
                      ]

given_missingfromaccount_when_transfer_then_exception :: TestTree
given_missingfromaccount_when_transfer_then_exception = testCase "Missing from account transfer throws exception" $ do
  let fromIban = "AT12 12345 01234567890"
      toIban   = "AT12 12345 01234567890"
      amount   = 123
      ref      = "Test Transfer"

      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [] []

  let ret = testApplication appData (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)

  expectAccountNotFound ret

given_missingtoaccount_when_transfer_then_exception :: TestTree
given_missingtoaccount_when_transfer_then_exception = testCase "Missing to account transfer throws exception" $ do
  let fromIban = "AT12 12345 01234567890"
      toIban   = "AT23 45678 01234567890"
      amount   = 123
      ref      = "Test Transfer"
      fromAcc  = mkTestAccount Giro

      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [(Iban fromIban, fromAcc)] []

  let ret = testApplication appData (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)

  expectAccountNotFound ret

given_fromgirotosavingsaccount_when_transfer_then_exception :: TestTree
given_fromgirotosavingsaccount_when_transfer_then_exception = testCase "Transfering from Giro to Savings throws exception" $ do
  fromCust <- toText <$> nextRandom
  toCust   <- toText <$> nextRandom

  let fromIban = "AT12 12345 01234567890"
      toIban   = "AT23 45678 01234567890"
      amount   = 123
      ref      = "Test Transfer"
      fromAcc  = mkTestAccountWith Giro fromCust fromIban 0
      toAcc    = mkTestAccountWith Savings toCust toIban 0

      now     = mkUTCTime 2021 03 01 0 0 0 
      appData = mkPureAppData now [(Iban fromIban, fromAcc), (Iban toIban, toAcc)] []


  let ret = testApplication appData (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)

  expectInvalidAccountOp ret "Transfer cannot happen with Savings account of different customers!"

given_fromsavingstogiroaccount_when_transfer_then_exception :: TestTree
given_fromsavingstogiroaccount_when_transfer_then_exception = testCase "Transfering from Savings to Giro throws exception" $ do
  fromCust <- toText <$> nextRandom
  toCust   <- toText <$> nextRandom

  let fromIban = "AT12 12345 01234567890"
      toIban   = "AT23 45678 01234567890"
      amount   = 123
      ref      = "Test Transfer"
      fromAcc  = mkTestAccountWith Savings fromCust fromIban 0
      toAcc    = mkTestAccountWith Giro toCust toIban 0

      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [(Iban fromIban, fromAcc), (Iban toIban, toAcc)] []

  let ret = testApplication appData (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)

  expectInvalidAccountOp ret "Transfer cannot happen with Savings account of different customers!"


given_amountLT5000_when_transfer_then_exception :: TestTree
given_amountLT5000_when_transfer_then_exception = testCase "Transfering more than 5000 between different customers throws exception" $ do
  fromCust <- toText <$> nextRandom
  toCust   <- toText <$> nextRandom

  let fromIban = "AT12 12345 01234567890"
      toIban   = "AT23 45678 01234567890"
      amount   = 5001
      ref      = "Test Transfer"
      fromAcc  = mkTestAccountWith Giro fromCust fromIban 0
      toAcc    = mkTestAccountWith Giro toCust toIban 0

      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [(Iban fromIban, fromAcc), (Iban toIban, toAcc)] []

  let ret = testApplication appData (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)

  expectInvalidAccountOp ret "Transfer between different customers cannot exceed 5000€!"


given_missingfromCustomer_when_transfer_then_exception :: TestTree
given_missingfromCustomer_when_transfer_then_exception = testCase "Missing from customer throws exception" $ do
  fromCustId <- nextRandom
  toCustId   <- nextRandom

  let fromIban = "AT12 12345 01234567890"
      toIban   = "AT23 45678 01234567890"
      amount   = 1234
      ref      = "Test Transfer"
      fromAcc  = mkTestAccountWith Giro (toText fromCustId) fromIban 0
      toAcc    = mkTestAccountWith Giro (toText toCustId) toIban 0

      toCust   = Customer (CustomerId toCustId) "Thomas Schwarz"

      now      = mkUTCTime 2021 03 01 0 0 0
      appData  = mkPureAppData
                  now
                  [(Iban fromIban, fromAcc), (Iban toIban, toAcc)]
                  [(CustomerId toCustId, toCust)]

  let ret = testApplication appData (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)

  expectCustomerNotFound ret


given_missingtoCustomer_when_transfer_then_exception :: TestTree
given_missingtoCustomer_when_transfer_then_exception = testCase "Missing to customer throws exception" $ do
  fromCustId <- nextRandom
  toCustId   <- nextRandom

  let fromIban = "AT12 12345 01234567890"
      toIban   = "AT23 45678 01234567890"
      amount   = 1234
      ref      = "Test Transfer"
      fromAcc  = mkTestAccountWith Giro (toText fromCustId) fromIban 0
      toAcc    = mkTestAccountWith Giro (toText toCustId) toIban 0

      fromCust = Customer (CustomerId fromCustId) "Jonathan Thaler"

      now      = mkUTCTime 2021 03 01 0 0 0
      appData  = mkPureAppData
                  now
                  [(Iban fromIban, fromAcc), (Iban toIban, toAcc)]
                  [(CustomerId fromCustId, fromCust)]

  let ret = testApplication appData (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)

  expectCustomerNotFound ret


given_accountsdifferentcustomers_when_transfer_then_returnTxLine :: TestTree
given_accountsdifferentcustomers_when_transfer_then_returnTxLine = testCase "Transfer between accounts of different customers results in TxLine" $ do
  fromCustId <- nextRandom
  toCustId   <- nextRandom

  let fromIban = "AT12 12345 01234567890"
      toIban   = "AT23 45678 01234567890"
      amount   = 123
      ref      = "Test Transfer"

      fromAcc  = mkTestAccountWith Giro (toText fromCustId) fromIban 0
      toAcc    = mkTestAccountWith Giro (toText toCustId) toIban 0

      fromCust = Customer (CustomerId fromCustId) "Jonathan Thaler"
      toCust   = Customer (CustomerId toCustId) "Thomas Schwarz"

      now      = mkUTCTime 2021 03 01 0 0 0
      appData  = mkPureAppData
                  now
                  [(Iban fromIban, fromAcc), (Iban toIban, toAcc)]
                  [(CustomerId fromCustId, fromCust), (CustomerId toCustId, toCust)]


      txDtoExpected = TXLineDTO toIban "Thomas Schwarz" ref (-amount) now

  let ret = testApplication appData (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)

  case ret of
    Left e              -> assertFailure $ "Did not expect exception " ++ show e
    (Right txDtoActual) -> assertEqual "TXLineDTOs do not match!" txDtoExpected txDtoActual

given_accountssamecustomer_when_transfer_then_returnTxLine :: TestTree
given_accountssamecustomer_when_transfer_then_returnTxLine = testCase "Transfer between accounts of same customer results in TxLine" $ do
  custId <- nextRandom

  let fromIban = "AT12 12345 01234567890"
      toIban   = "AT23 45678 01234567890"
      amount   = 5001
      ref      = "Test Transfer"

      fromAcc  = mkTestAccountWith Giro (toText custId) fromIban 10000
      toAcc    = mkTestAccountWith Savings (toText custId) toIban 0

      cust     = Customer (CustomerId custId) "Jonathan Thaler"

      now      = mkUTCTime 2021 03 01 0 0 0
      appData  = mkPureAppData
                  now
                  [(Iban fromIban, fromAcc), (Iban toIban, toAcc)]
                  [(CustomerId custId, cust)]

      txDtoExpected = TXLineDTO toIban "Jonathan Thaler" ref (-amount) now

  let ret = testApplication appData (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)

  case ret of
    Left e              -> assertFailure $ "Did not expect exception " ++ show e
    (Right txDtoActual) -> assertEqual "TXLineDTOs do not match!" txDtoExpected txDtoActual
