{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-} -- for more convenience defining mocks
{-# OPTIONS_GHC -fno-warn-incomplete-patterns     #-} -- for more convenience defining mocks
{-# LANGUAGE LambdaCase #-}
module Test.Application.FreeMSF.Transfer where

import           Application.FreeMSF.Banking as Banking
import           Application.DTO
import           Control.Monad.Except
import           Data.UUID
import           Data.UUID.V4              (nextRandom)
import           Domain.FreeMSF.Account.Api
import           Domain.FreeMSF.Account.Repository
import           Domain.FreeMSF.Customer.Customer
import           Domain.FreeMSF.Customer.Repository
import           Domain.Types
import           Test.Application.FreeMSF.Runner
import           Test.Application.Utils
import           Test.Domain.FreeMSF.Account.Data
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils.Time

transfer_freemsf_tests :: TestTree
transfer_freemsf_tests = testGroup "Transfer FreeMSF Tests"
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

  let accountRepoMock = (\(FindAccountByIban _ cont) -> return $ cont Nothing)

  let ret = testApplication
              (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)
              accountRepoMock
              undefined
              undefined

  expectAccountNotFound ret

given_missingtoaccount_when_transfer_then_exception :: TestTree
given_missingtoaccount_when_transfer_then_exception = testCase "Missing to account transfer throws exception" $ do
  let fromIban = "AT12 12345 01234567890"
      toIban   = "AT23 45678 01234567890"
      amount   = 123
      ref      = "Test Transfer"
      fromAcc  = mkTestAccount Giro

  let accountRepoMock = (\(FindAccountByIban (Iban iban) cont) ->
                              if fromIban == iban
                                then return $ cont $ Just fromAcc
                                else return $ cont Nothing)

  let ret = testApplication
              (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)
              accountRepoMock
              undefined
              undefined

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

  let accountRepoMock = (\(FindAccountByIban (Iban iban) cont) ->
                              if fromIban == iban
                                then return $ cont $ Just fromAcc
                                else return $ cont $ Just toAcc)

  let ret = testApplication
              (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)
              accountRepoMock
              undefined
              undefined

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

  let accountRepoMock = (\(FindAccountByIban (Iban iban) cont) ->
                              if fromIban == iban
                                then return $ cont $ Just fromAcc
                                else return $ cont $ Just toAcc)

  let ret = testApplication
              (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)
              accountRepoMock
              undefined
              undefined

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

  let accountRepoMock = (\(FindAccountByIban (Iban iban) cont) ->
                              if fromIban == iban
                                then return $ cont $ Just fromAcc
                                else return $ cont $ Just toAcc)

  let ret = testApplication
              (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)
              accountRepoMock
              undefined
              undefined

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

      toCust   = customer (CustomerId toCustId) "Thomas Schwarz"

  let accountRepoMock = (\(FindAccountByIban (Iban iban) cont) ->
                              if fromIban == iban
                                then return $ cont $ Just fromAcc
                                else return $ cont $ Just toAcc)

  let customerRepoMock = (\(FindCustomerById (CustomerId cid) cont) ->
                              if fromCustId == cid
                                then return $ cont Nothing
                                else return $ cont $ Just toCust)

  let ret = testApplication
              (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)
              accountRepoMock
              customerRepoMock
              undefined

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

      fromCust = customer (CustomerId fromCustId) "Jonathan Thaler"

  let accountRepoMock = (\(FindAccountByIban (Iban iban) cont) ->
                              if fromIban == iban
                                then return $ cont $ Just fromAcc
                                else return $ cont $ Just toAcc)

  let customerRepoMock = (\(FindCustomerById (CustomerId cid) cont) ->
                              if fromCustId == cid
                                then return $ cont $ Just fromCust
                                else return $ cont Nothing)

  let ret = testApplication
              (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)
              accountRepoMock
              customerRepoMock
              undefined

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

      fromCust = customer (CustomerId fromCustId) "Jonathan Thaler"
      toCust   = customer (CustomerId toCustId) "Thomas Schwarz"

      now           = mkUTCTime 2021 03 01 0 0 0
      txDtoExpected = TXLineDTO toIban "Thomas Schwarz" ref (-amount) now

  let accountRepoMock = (\(FindAccountByIban (Iban iban) cont) ->
                              if fromIban == iban
                                then return $ cont $ Just fromAcc
                                else return $ cont $ Just toAcc)

  let customerRepoMock = (\(FindCustomerById (CustomerId cid) cont) ->
                              if fromCustId == cid
                                then return $ cont $ Just fromCust
                                else return $ cont $ Just toCust)

  let accAggMock =  \case
                      PersistTXLine _ m i name r cont -> return $ cont $ TXLine m i name r now
                      UpdateBalance _ _ ret -> return ret

  let ret = testApplication
              (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)
              accountRepoMock
              customerRepoMock
              accAggMock

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

      cust = customer (CustomerId custId) "Jonathan Thaler"

      now           = mkUTCTime 2021 03 01 0 0 0
      txDtoExpected = TXLineDTO toIban "Jonathan Thaler" ref (-amount) now

  let accountRepoMock = (\(FindAccountByIban (Iban iban) cont) ->
                              if fromIban == iban
                                then return $ cont $ Just fromAcc
                                else return $ cont $ Just toAcc)

  let customerRepoMock = (\(FindCustomerById _ cont) -> return $ cont $ Just cust)

  let accAggMock =  \case
                      PersistTXLine _ m i name r cont -> return $ cont $ TXLine m i name r now
                      UpdateBalance _ _ ret -> return ret

  let ret = testApplication
              (runExceptT $ Banking.transferTransactional fromIban toIban amount ref)
              accountRepoMock
              customerRepoMock
              accAggMock

  case ret of
    Left e              -> assertFailure $ "Did not expect exception " ++ show e
    (Right txDtoActual) -> assertEqual "TXLineDTOs do not match!" txDtoExpected txDtoActual
