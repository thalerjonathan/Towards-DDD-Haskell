{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Application.Tagless.Banking
  ( Exception (..)

  , createCustomer
  , createAccount
  , getAllCustomers
  , getCustomer
  , getAccount
  , deposit
  , withdraw
  , transferTransactional
  , transferEventual
  , processDomainEvent
  ) where

import           Application.DTO
import           Application.Tagless.Layer
import           Application.Events             as Events
import           Application.Exceptions
import           Control.Monad.Except
import qualified Data.Text                      as T
import           Domain.Tagless.Account
import           Domain.Tagless.Account.Repository
import           Domain.Tagless.Customer
import           Domain.Tagless.Customer.Repository
import           Domain.Types

{-
import qualified Data.Text.Lazy              as TL
import           Data.Aeson.Text             as Aeson
import           Data.Time.Clock
-}

data TransferType = Transactional | Eventual

type ApplicationExcept m a = ExceptT Exception m a

createCustomer :: (ApplicationLayer m, CustomerRepo m)
               => T.Text
               -> m T.Text
createCustomer name = do
  logDebug "createCustomer"
  cId <- CustomerId <$> nextUUID
  _c <- addCustomer cId name
  return $ customerIdToText cId

createAccount :: (ApplicationLayer m, AccountRepo m, CustomerRepo m)
              => T.Text
              -> T.Text
              -> Double
              -> T.Text
              -> ApplicationExcept m ()
createAccount owner iban balance t = do
  lift $ logDebug "createAccount"
  let cid = customerIdFromTextUnsafe owner
  _ <- tryMaybe_ (lift $ findCustomerById cid) CustomerNotFound
  let aType = read (T.unpack t) :: AccountType
  _ <- lift $ addAccount cid balance (Iban iban) aType
  return ()

getAllCustomers :: (ApplicationLayer m, CustomerRepo m)
                => m [CustomerDetailsDTO]
getAllCustomers = do
  logDebug "getAllCustomers"
  cs <- allCustomers
  return $ map customerToDetailsDTO cs

getCustomer :: (ApplicationLayer m, CustomerRepo m, AccountRepo m)
            => T.Text
            -> ApplicationExcept m CustomerDTO
getCustomer cIdStr = do
  lift $ logDebug "getCustomer"
  let cid = customerIdFromTextUnsafe cIdStr
  c  <- tryMaybe_ (lift $ findCustomerById cid) CustomerNotFound
  as <- lift $ findAccountsForOwner cid
  return $ customerToDTO c as

getAccount :: (ApplicationLayer m, AccountRepo m, AccountAggregate m)
           => T.Text
           -> ApplicationExcept m AccountDTO
getAccount i = do
  lift $ logDebug "getAccount"
  a  <- tryMaybe_ (lift $ findAccountByIban (Iban i)) AccountNotFound
  txs <- lift $ accountTxLines a
  return $ accountToDTO a txs

deposit :: (ApplicationLayer m, AccountRepo m, AccountAggregate m)
        => T.Text
        -> Double
        -> ApplicationExcept m TXLineDTO
deposit ibanStr amount = do
  lift $ logDebug "deposit"
  a         <- tryMaybe_ (lift $ findAccountByIban (Iban ibanStr)) AccountNotFound
  (tx, _a') <- withExceptT InvalidAccountOperation (accountDeposit a amount)
  return $ txLineToDTO tx

withdraw :: (ApplicationLayer m, AccountRepo m, AccountAggregate m)
        => T.Text
        -> Double
        -> ApplicationExcept m TXLineDTO
withdraw ibanStr amount = do
  lift $ logDebug "withdraw"
  a         <- tryMaybe_ (lift $ findAccountByIban (Iban ibanStr)) AccountNotFound
  (tx, _a') <- withExceptT InvalidAccountOperation (accountWithdraw a amount)
  return $ txLineToDTO tx

transferEventual :: (ApplicationLayer m, AccountRepo m, CustomerRepo m, AccountAggregate m)
                 => T.Text
                 -> T.Text
                 -> Double
                 -> T.Text
                 -> ApplicationExcept m TXLineDTO
transferEventual fromIban toIban amount reference = do
  lift $ logDebug "transferEventual"
  checkAndPerformTransfer fromIban toIban amount reference Eventual

transferTransactional :: (ApplicationLayer m, AccountRepo m, CustomerRepo m, AccountAggregate m)
                      => T.Text
                      -> T.Text
                      -> Double
                      -> T.Text
                      -> ApplicationExcept m TXLineDTO
transferTransactional fromIban toIban amount reference = do
  lift $ logDebug "transferTransactional"
  checkAndPerformTransfer fromIban toIban amount reference Transactional

checkAndPerformTransfer :: (ApplicationLayer m, AccountRepo m, CustomerRepo m, AccountAggregate m)
                        => T.Text
                        -> T.Text
                        -> Double
                        -> T.Text
                        -> TransferType
                        -> ApplicationExcept m TXLineDTO
checkAndPerformTransfer fromIban toIban amount reference txType = do
  fromAccount <- tryMaybe_ (lift $ findAccountByIban (Iban fromIban)) AccountNotFound
  toAccount   <- tryMaybe_ (lift $ findAccountByIban (Iban toIban)) AccountNotFound

  -- NOTE: this check is similar to a domain service
  withExceptT InvalidAccountOperation (guardWith_ (checkTransfer fromAccount toAccount amount))

  case txType of
    Transactional -> performTransferTransactional fromAccount toAccount amount reference
    Eventual      -> performTransferEventual fromAccount toAccount amount reference

-- NOTE: this can be seen as Domain Service functionality
checkTransfer :: Account
              -> Account
              -> Double
              -> Maybe T.Text
checkTransfer fromAccount toAccount amount = do
  let fromOwner = _aOwner fromAccount
  let toOwner   = _aOwner toAccount

  -- same owner, anything goes, no restrictions
  if fromOwner == toOwner
    then Nothing
    -- not same owner, can only transfer between giros and max 5000 amount
    else do
      let fromType = _aType fromAccount
      let toType   = _aType toAccount

      if fromType == Savings || toType == Savings
        then Just "Transfer cannot happen with Savings account of different customers!"
        else if amount > 5000
          then Just "Transfer between different customers cannot exceed 5000€!"
          else Nothing

performTransferTransactional :: (ApplicationLayer m, CustomerRepo m, AccountAggregate m)
                             => Account
                             -> Account
                             -> Double
                             -> T.Text
                             -> ApplicationExcept m TXLineDTO
performTransferTransactional fromAccount toAccount amount reference = do
  let fromOwner = _aOwner fromAccount
  fromCustomer  <- tryMaybe_ (lift $ findCustomerById fromOwner) CustomerNotFound
  let toOwner   = _aOwner toAccount
  toCustomer    <- tryMaybe_ (lift $ findCustomerById toOwner) CustomerNotFound

  let fromName = _custName fromCustomer
  let toName   = _custName toCustomer
  let fromIban = _aIban fromAccount
  let toIban   = _aIban toAccount

  (fromTxLine, _fromAccount') <- withExceptT InvalidAccountOperation (accountTransferTo fromAccount toIban amount toName reference)
  (_toTxLine, _toAccount')    <- withExceptT InvalidAccountOperation (accountReceiveFrom toAccount fromIban amount fromName reference)
  return $ txLineToDTO fromTxLine

performTransferEventual :: (ApplicationLayer m, CustomerRepo m, AccountAggregate m)
                        => Account
                        -> Account
                        -> Double
                        -> T.Text
                        -> ApplicationExcept m TXLineDTO
performTransferEventual fromAccount toAccount amount reference = do
  let fromOwner = _aOwner fromAccount
  _  <- tryMaybe_ (lift $ findCustomerById fromOwner) CustomerNotFound
  let toOwner   = _aOwner toAccount
  toCustomer <- tryMaybe_ (lift $ findCustomerById toOwner) CustomerNotFound
  let toName   = _custName toCustomer
  let toIban@(Iban toIbanStr) = _aIban toAccount
  let (Iban fromIbanStr) = _aIban fromAccount

  (fromTxLine, _fromAccount') <- withExceptT InvalidAccountOperation (accountTransferTo fromAccount toIban amount toName reference)

  let evtData = TransferSentEventData {
      transferSentEventAmount            = amount
    , transferSentEventReference         = reference
    , transferSentEventSendingCustomer   = customerIdToText fromOwner
    , transferSentEventReceivingCustomer = customerIdToText toOwner
    , transferSentEventSendingAccount    = fromIbanStr
    , transferSentEventReceivingAccount  = toIbanStr
    }

  lift $ persistDomainEvent (Events.TransferSent evtData)
  return $ txLineToDTO fromTxLine

processDomainEvent :: (ApplicationLayer m, CustomerRepo m, AccountRepo m, AccountAggregate m)
                   => DomainEvent -> m ()
processDomainEvent (Events.TransferSent evt)   = void (runExceptT (transferSent evt))
processDomainEvent (Events.TransferFailed evt) = void (runExceptT (transferFailed evt))

transferSent :: (ApplicationLayer m, CustomerRepo m, AccountRepo m, AccountAggregate m)
             => TransferSentEventData 
             -> ExceptT () m ()
transferSent evt = do
    let fromIban  = transferSentEventSendingAccount evt
        toIban    = transferSentEventReceivingAccount evt
        amount    = transferSentEventAmount evt
        reference = transferSentEventReference evt

    fromAccount <- tryMaybeM_
                    (lift $ findAccountByIban (Iban fromIban))
                    (lift $ transferSentFailed evt "Could not find sending Account" >> return ())

    toAccount <- tryMaybeM_
                    (lift $ findAccountByIban (Iban toIban))
                    (lift $ transferSentFailed evt "Could not find sending Account" >> return ())

    let fromOwner = _aOwner fromAccount

    _ <- tryMaybeM_
          (lift $ findCustomerById fromOwner)
          (lift $ transferSentFailed evt "Could not find sending customer" >> return ())

    let toOwner = _aOwner toAccount

    fromCustomer <- tryMaybeM_
                      (lift $ findCustomerById toOwner)
                      (lift $ transferSentFailed evt "Could not find receiving customer" >> return ())

    let fromName = _custName fromCustomer

    (_fromTxLine, _fromAccount') <- withExceptT (const ()) (accountReceiveFrom toAccount (Iban fromIban) amount fromName reference)
    return ()
  where
    transferSentFailed :: ApplicationLayer m => TransferSentEventData -> T.Text -> m ()
    transferSentFailed evtSentData err = do
      let evtFailedData = TransferFailedEventData {
          transferFailedEventError             = err
        , transferFailedEventAmount            = transferSentEventAmount evtSentData
        , transferFailedEventReference         = transferSentEventReference evtSentData
        , transferFailedEventSendingCustomer   = transferSentEventSendingCustomer evtSentData
        , transferFailedEventReceivingCustomer = transferSentEventReceivingCustomer evtSentData
        , transferFailedEventSendingAccount    = transferSentEventSendingAccount evtSentData
        , transferFailedEventReceivingAccount  = transferSentEventReceivingAccount evtSentData
        }

      persistDomainEvent (Events.TransferFailed evtFailedData)

transferFailed :: (ApplicationLayer m, CustomerRepo m, AccountRepo m, AccountAggregate m)
               => TransferFailedEventData 
               -> ExceptT () m ()
transferFailed evt = do
  let fromIban  = transferFailedEventSendingAccount evt
      toIban    = transferFailedEventReceivingAccount evt
      amount    = transferFailedEventAmount evt
      reference = transferFailedEventReference evt

  fromAccount <- tryMaybeM_
                  (lift $ findAccountByIban (Iban fromIban))
                  (lift $ logError "Processing TransferFailed event failed: could not find sending Account!" >> return ())

  toAccount <- tryMaybeM_
                  (lift $ findAccountByIban (Iban toIban))
                  (lift $ logError "Processing TransferFailed event failed: could not find receiving Account!" >> return ())

  let fromOwner = _aOwner fromAccount

  _ <- tryMaybeM_
        (lift $ findCustomerById fromOwner)
        (lift $ logError "Processing TransferFailed event failed: could not find sending Customer!" >> return ())

  let toOwner = _aOwner toAccount

  toCustomer <- tryMaybeM_
                  (lift $ findCustomerById toOwner)
                  (lift $ logError "Processing TransferFailed event failed: could not find receiving Customer!" >> return ())

  let toName = _custName toCustomer

  (_fromTxLine, _fromAccount') <- withExceptT (const ()) (accountReceiveFrom fromAccount (Iban fromIban) amount toName ("Transfer failed: " <> reference))
  return ()

customerToDetailsDTO :: Customer -> CustomerDetailsDTO
customerToDetailsDTO (Customer cid cname) = CustomerDetailsDTO
  { customerDetailsId   = customerIdToText cid
  , customerDetailsName = cname
  }

customerToDTO :: Customer -> [Account] -> CustomerDTO
customerToDTO c as = CustomerDTO
  { customerDetails        = customerToDetailsDTO c
  , customerAccountDetails = map accountToDetailsDTO as
  }

accountToDetailsDTO :: Account -> AccountDetailsDTO
accountToDetailsDTO (Account _ _ (Iban aIban) aBalance aType) = AccountDetailsDTO
  { accountDetailIban    = aIban
  , accountDetailBalance = aBalance
  , accountDetailType    = T.pack $ show aType
  }

accountToDTO :: Account -> [TxLine] -> AccountDTO
accountToDTO a txs = AccountDTO
  { accountDetails = accountToDetailsDTO a
  , accountTXLines = map txLineToDTO txs
  }

txLineToDTO :: TxLine -> TXLineDTO
txLineToDTO (TxLine (Iban txIban) txName txRef txAmount txTime) = TXLineDTO
  { txLineIban      = txIban
  , txLineName      = txName
  , txLineReference = txRef
  , txLineAmount    = txAmount
  , txLineTime      = txTime
  }

logError :: ApplicationLayer m => T.Text -> m ()
logError = logging Error

logDebug :: ApplicationLayer m => T.Text -> m ()
logDebug = logging Debug