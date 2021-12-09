module Application.BankingDomain where

import           Application.DTO
import           Application.DomainEvents   as DomainEvents
import           Application.Exceptions
import           Application.Layer
import           Control.Monad.Except
import           Data.Text                  as T
import           Domain.Account.Api
import           Domain.Account.Repository
import           Domain.Customer.Customer
import           Domain.Customer.Repository
import           Domain.Types

data TransferType = Transactional | Eventual

type Application          = ApplicationLayer
type ApplicationExcept ex = ExceptT ex Application

createCustomer :: T.Text -> Application T.Text
createCustomer name = do
  cId <- CustomerId <$> nextUUID
  _c <- runRepo $ customerRepo $ addCustomer cId name
  return $ customerIdToText cId

createAccount :: T.Text
              -> T.Text
              -> Double
              -> T.Text
              -> ApplicationExcept Exception ()
createAccount owner iban balance t = do
  let cid = customerIdFromTextUnsafe owner
  _ <- tryMaybe (runRepo $ customerRepo $ findCustomerById cid) CustomerNotFound
  let aType = read (T.unpack t) :: AccountType
  _ <- lift $ runRepo $ accountRepo $ addAccount cid balance (Iban iban) aType
  return ()

getAllCustomers :: Application [CustomerDetailsDTO]
getAllCustomers = do
  cs   <- runRepo $ customerRepo $ allCustomers
  runAggregate $ customerAggregate $ mapM customerToDetailsDTO cs
  
getCustomer :: T.Text -> ApplicationExcept Exception CustomerDTO
getCustomer cIdStr = do
  let cid = customerIdFromTextUnsafe cIdStr
  c  <- tryMaybe (runRepo $ customerRepo $ findCustomerById cid) CustomerNotFound
  as <- lift $ runRepo $ accountRepo $ findAccountsForOwner cid

  custDetails <- lift $ runAggregate $ customerAggregate $ customerToDetailsDTO c
  asDetails   <- lift $ runAggregate $ accountAggregate $ mapM accountToDetailsDTO as

  return $ CustomerDTO {
      customerDetails        = custDetails
    , customerAccountDetails = asDetails
    }

getAccount :: T.Text -> ApplicationExcept Exception AccountDTO
getAccount ibanStr = do
  a   <- tryMaybe (runRepo $ accountRepo $ findAccountByIban (Iban ibanStr)) AccountNotFound
  lift $ runAggregate $ accountAggregate $ accountToDTO a

deposit :: T.Text
        -> Double
        -> ApplicationExcept Exception TXLineDTO
deposit ibanStr amount = do
  a <- tryMaybe (runRepo $ accountRepo $ findAccountByIban (Iban ibanStr)) AccountNotFound
  (_, ret, _) <- lift $ runAggregate $ accountAggregate $ Domain.Account.Api.deposit a amount
  case ret of
    (DepositResult (Left err)) -> throwError $ InvalidAccountOperation err
    (DepositResult (Right tx)) -> return $ txLineToDTO tx
    _ -> error "Unexpected result of Domain.Account.deposit"

withdraw :: T.Text
         -> Double
         -> ApplicationExcept Exception TXLineDTO
withdraw ibanStr amount = do
  a <- tryMaybe (runRepo $ accountRepo $ findAccountByIban (Iban ibanStr)) AccountNotFound
  (_, ret, _) <- lift $ runAggregate $ accountAggregate $ Domain.Account.Api.withdraw a amount
  case ret of
    (WithdrawResult (Left err)) -> throwError $ InvalidAccountOperation err
    (WithdrawResult (Right tx)) -> return $ txLineToDTO tx
    _ -> error "Unexpected result of Domain.Account.withdraw"

transferEventual :: T.Text
                 -> T.Text
                 -> Double
                 -> T.Text
                 -> ApplicationExcept Exception TXLineDTO
transferEventual fromIban toIban amount reference =
  checkAndPerformTransfer fromIban toIban amount reference Eventual

transferTransactional :: T.Text
                      -> T.Text
                      -> Double
                      -> T.Text
                      -> ApplicationExcept Exception TXLineDTO
transferTransactional fromIban toIban amount reference =
  checkAndPerformTransfer fromIban toIban amount reference Transactional

checkAndPerformTransfer :: T.Text
                        -> T.Text
                        -> Double
                        -> T.Text
                        -> TransferType
                        -> ApplicationExcept Exception TXLineDTO
checkAndPerformTransfer fromIban toIban amount reference txType = do
  fromAccount <- tryMaybe (runRepo $ accountRepo $ findAccountByIban (Iban fromIban)) AccountNotFound 
  toAccount   <- tryMaybe (runRepo $ accountRepo $ findAccountByIban (Iban toIban)) AccountNotFound

  -- NOTE: this check is similar to a domain service
  guardWithM (runAggregate $ accountAggregate $ checkTransfer fromAccount toAccount amount) InvalidAccountOperation

  case txType of
    Transactional -> performTransferTransactional fromAccount toAccount amount reference
    Eventual      -> performTransferEventual fromAccount toAccount amount reference

-- NOTE: this can be seen as Domain Service functionality
checkTransfer :: Account
              -> Account
              -> Double
              -> AccountProgram (Maybe T.Text)
checkTransfer fromAccount toAccount amount = do
  fromOwner <- getOwner fromAccount
  toOwner   <- getOwner toAccount

  -- same owner, anything goes, no restrictions
  if fromOwner == toOwner
    then return Nothing
    -- not same owner, can only transfer between giros and max 5000 amount
    else do
      fromType <- getType fromAccount
      toType   <- getType toAccount

      if fromType == Savings || toType == Savings
        then return $ Just "Transfer cannot happen with Savings account of different customers!"
        else if amount > 5000
          then return $ Just "Transfer between different customers cannot exceed 5000€!"
          else return Nothing

performTransferTransactional :: Account
                             -> Account
                             -> Double
                             -> T.Text
                             -> ApplicationExcept Exception TXLineDTO
performTransferTransactional fromAccount toAccount amount reference = do
  fromOwner    <- lift $ runAggregate $ accountAggregate $ getOwner fromAccount
  fromCustomer <- tryMaybe (runRepo $ customerRepo $ findCustomerById fromOwner) CustomerNotFound
  toOwner      <- lift $ runAggregate $ accountAggregate $ getOwner toAccount
  toCustomer   <- tryMaybe (runRepo $ customerRepo $ findCustomerById toOwner) CustomerNotFound

  fromName <- lift $ runAggregate $ customerAggregate $ getName fromCustomer
  toName   <- lift $ runAggregate $ customerAggregate $ getName toCustomer
  fromIban <- lift $ runAggregate $ accountAggregate $ getIban fromAccount
  toIban   <- lift $ runAggregate $ accountAggregate $ getIban toAccount

  (_, retTo, _) <- lift $ runAggregate $ accountAggregate $ transferTo fromAccount toIban amount toName reference
  case retTo of
    (TransferToResult (Right fromTxLine)) -> do
      (_, retFrom, _) <- lift $ runAggregate $ accountAggregate $ receiveFrom toAccount fromIban amount fromName reference
      case retFrom of
        (ReceiveFromResult _) -> do
          return $ txLineToDTO fromTxLine
        _ ->
          error "unexpected result"
    (TransferToResult (Left err)) ->
      throwError $ InvalidAccountOperation err
    _ ->
      error "unexpected result"

performTransferEventual :: Account
                        -> Account
                        -> Double
                        -> T.Text
                        -> ApplicationExcept Exception TXLineDTO
performTransferEventual fromAccount toAccount amount reference = do
  fromOwner  <- lift $ runAggregate $ accountAggregate $ getOwner fromAccount
  _          <- tryMaybe (runRepo $ customerRepo $ findCustomerById fromOwner) CustomerNotFound
  toOwner    <- lift $ runAggregate $ accountAggregate $ getOwner toAccount
  toCustomer <- tryMaybe (runRepo $ customerRepo $ findCustomerById toOwner) CustomerNotFound
  toName     <- lift $ runAggregate $ customerAggregate $ getName toCustomer
  toIban@(Iban toIbanTx) <- lift $ runAggregate $ accountAggregate $ getIban toAccount

  (_, ret, _) <- lift $ runAggregate $ accountAggregate $ transferTo fromAccount toIban amount toName reference
  case ret of
    (TransferToResult (Right txLine)) -> do
      (Iban fromIban) <- lift $ runAggregate $ accountAggregate $ getIban fromAccount

      let evtData = TransferSentEventData {
          transferSentEventAmount            = amount
        , transferSentEventReference         = reference
        , transferSentEventSendingCustomer   = customerIdToText fromOwner
        , transferSentEventReceivingCustomer = customerIdToText toOwner
        , transferSentEventSendingAccount    = fromIban
        , transferSentEventReceivingAccount  = toIbanTx
        }

      lift $ persistDomainEvent (DomainEvents.TransferSent evtData)
      return $ txLineToDTO txLine
    _ ->
      error "Unexpected result"

processDomainEvent :: DomainEvent -> Application ()
processDomainEvent (DomainEvents.TransferSent evt)   = void (runExceptT (transferSent evt))
processDomainEvent (DomainEvents.TransferFailed evt) = void (runExceptT (transferFailed evt))

transferSent :: TransferSentEventData -> ApplicationExcept () ()
transferSent evt = do
    let fromIban  = transferSentEventSendingAccount evt
        toIban    = transferSentEventReceivingAccount evt
        amount    = transferSentEventAmount evt
        reference = transferSentEventReference evt

    fromAccount <- tryMaybeM
                    (runRepo $ accountRepo $ findAccountByIban (Iban fromIban))
                    (transferSentFailed evt "Could not find sending Account")

    toAccount <- tryMaybeM
                    (runRepo $ accountRepo $ findAccountByIban (Iban toIban))
                    (transferSentFailed evt "Could not find sending Account")

    fromOwner <- lift $ runAggregate $ accountAggregate $ getOwner fromAccount

    _ <- tryMaybeM
          (runRepo $ customerRepo $ findCustomerById fromOwner)
          (transferSentFailed evt "Could not find sending customer")

    toOwner <- lift $ runAggregate $ accountAggregate $ getOwner toAccount

    fromCustomer <- tryMaybeM
                      (runRepo $ customerRepo $ findCustomerById toOwner)
                      (transferSentFailed evt "Could not find receiving customer")

    fromName <- lift $ runAggregate $ customerAggregate $ getName fromCustomer

    (_, retFrom, _) <- lift $ runAggregate $ accountAggregate $ receiveFrom toAccount (Iban fromIban) amount fromName reference
    case retFrom of
      (ReceiveFromResult _) -> do
        return ()
      _ ->
        error "unexpected result"
  where
    transferSentFailed :: TransferSentEventData -> T.Text -> Application ()
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

      persistDomainEvent (DomainEvents.TransferFailed evtFailedData)

transferFailed :: TransferFailedEventData -> ApplicationExcept () ()
transferFailed evt = do
  let fromIban  = transferFailedEventSendingAccount evt
      toIban    = transferFailedEventReceivingAccount evt
      amount    = transferFailedEventAmount evt
      reference = transferFailedEventReference evt

  fromAccount <- tryMaybeM
                  (runRepo $ accountRepo $ findAccountByIban (Iban fromIban))
                  (logError "Processing TransferFailed event failed: could not find sending Account!")

  toAccount <- tryMaybeM
                  (runRepo $ accountRepo $ findAccountByIban (Iban toIban))
                  (logError "Processing TransferFailed event failed: could not find receiving Account!")

  fromOwner <- lift $ runAggregate $ accountAggregate $ getOwner fromAccount

  _ <- tryMaybeM
        (runRepo $ customerRepo $ findCustomerById fromOwner)
        (logError "Processing TransferFailed event failed: could not find sending Customer!")

  toOwner <- lift $ runAggregate $ accountAggregate $ getOwner toAccount

  toCustomer <- tryMaybeM
                  (runRepo $ customerRepo $ findCustomerById toOwner)
                  (logError "Processing TransferFailed event failed: could not find receiving Customer!")

  toName <- lift $ runAggregate $ customerAggregate $ getName toCustomer

  (_, retFrom, _) <- lift $ runAggregate $ accountAggregate $ receiveFrom fromAccount (Iban fromIban) amount toName ("Transfer failed: " <> reference)
  case retFrom of
    (ReceiveFromResult _) -> do
      return ()
    _ ->
      error "unexpected result"

-------------------------------------------------------------------------------
-- Aggregate to DTO mappers
-------------------------------------------------------------------------------

customerToDetailsDTO :: Customer -> CustomerProgram CustomerDetailsDTO
customerToDetailsDTO c = do
  cname <- getName c
  cid   <- getDomainId c

  return $ CustomerDetailsDTO
    { customerDetailsId   = T.pack $ show cid
    , customerDetailsName = cname
    }

accountToDTO :: Account -> AccountProgram AccountDTO
accountToDTO  a
  = AccountDTO
      <$> accountToDetailsDTO a
      <*> (Prelude.map txLineToDTO <$> getTXLines a)

accountToDetailsDTO :: Account -> AccountProgram AccountDetailsDTO
accountToDetailsDTO a = do
  (Iban i) <- getIban a
  b        <- getBalance a
  accType  <- getType a

  return $ AccountDetailsDTO
    { accountDetailIban    = i
    , accountDetailBalance = b
    , accountDetailType    = T.pack $ show accType
    }

txLineToDTO :: TXLine -> TXLineDTO
txLineToDTO (TXLine a (Iban i) n ref t) = TXLineDTO
  { txLineIban      = i
  , txLineName      = n
  , txLineReference = ref
  , txLineAmount    = a
  , txLineTime      = t
  }
