module Application.BankingDomain where

import           Application.DTO
import           Application.DomainEvents   as DomainEvents
import           Application.Exceptions
import           Data.Maybe
import           Data.Text                  as T
import           Data.UUID
import           Domain.Account.Api
import           Domain.Account.Repository
import           Domain.Application
import           Domain.Customer.Customer
import           Domain.Customer.Repository
import           Domain.Types

-- TODO: create customer
-- TODO: create account

data TransferType = Transactional | Eventual

getAllCustomers :: Application [CustomerDetailsDTO]
getAllCustomers = do
    cs <- runRepo $ customerRepo $ allCustomers
    dtos <- runAggregate $ customerAggregate $ mapM customerToDetailsDTO cs
    return dtos

getCustomer :: T.Text -> Application (Either Exception CustomerDTO)
getCustomer cIdStr = do
    let cid = customerIdFromTextUnsafe cIdStr

    mc <- runRepo $ customerRepo $ findCustomerById cid
    case mc of
      Nothing -> return $ Left CustomerNotFound
      (Just c) -> do
        as <- runRepo $ accountRepo $ findAccountsForOwner cid

        custDetails <- runAggregate $ customerAggregate $ customerToDetailsDTO c
        asDetails   <- runAggregate $ accountAggregate $ mapM accountToDetailsDTO as

        let dto = CustomerDTO {
            customerDetails        = custDetails
          , customerAccountDetails = asDetails
          }

        return $ Right dto

getAccount :: T.Text -> Application (Either Exception AccountDTO)
getAccount ibanStr = do
  ma <- runRepo $ accountRepo $ findAccountByIban (Iban ibanStr)
  case ma of
    Nothing -> return $ Left AccountNotFound
    (Just a) -> do
      ret <- runAggregate $ accountAggregate $ accountToDTO a
      return $ Right ret

deposit :: T.Text
        -> Double
        -> Application (Either Exception TXLineDTO)
deposit i amount = do
  ma <- runRepo $ accountRepo $ findAccountByIban (Iban i)
  case ma of
    Nothing -> return $ Left AccountNotFound
    (Just a) -> do
      (_, ret, _) <- runAggregate $ accountAggregate $ Domain.Account.Api.deposit a amount
      case ret of
        (DepositResult (Left err)) -> return $ Left $ InvalidAccountOperation err
        (DepositResult (Right tx)) -> return $ Right $ txLineToDTO tx
        _ -> error "Unexpected result of Domain.Account.deposit"

withdraw :: T.Text
         -> Double
         -> Application (Either Exception TXLineDTO)
withdraw i amount = do
  ma <- runRepo $ accountRepo $ findAccountByIban (Iban i)
  case ma of
    Nothing -> return $ Left AccountNotFound
    (Just a) -> do
      (_, ret, _) <- runAggregate $ accountAggregate $ Domain.Account.Api.withdraw a amount
      case ret of
        (WithdrawResult (Left err)) -> return $ Left $ InvalidAccountOperation err
        (WithdrawResult (Right tx)) -> return $ Right $ txLineToDTO tx
        _ -> error "Unexpected result of Domain.Account.withdraw"

transferEventual :: T.Text
                 -> T.Text
                 -> Double
                 -> T.Text
                 -> Application (Either Exception TXLineDTO)
transferEventual fromIban toIban amount reference =
  checkAndPerformTransfer fromIban toIban amount reference Eventual

transferTransactional :: T.Text
                      -> T.Text
                      -> Double
                      -> T.Text
                      -> Application (Either Exception TXLineDTO)
transferTransactional fromIban toIban amount reference =
  checkAndPerformTransfer fromIban toIban amount reference Transactional

checkAndPerformTransfer :: T.Text
                        -> T.Text
                        -> Double
                        -> T.Text
                        -> TransferType
                        -> Application (Either Exception TXLineDTO)
checkAndPerformTransfer fromIban toIban amount reference txType = do
  mFrom <- runRepo $ accountRepo $ findAccountByIban (Iban fromIban)
  case mFrom of
    Nothing -> return $ Left AccountNotFound
    (Just fromAccount) -> do
      mTo <- runRepo $ accountRepo $ findAccountByIban (Iban toIban)
      case mTo of
        Nothing -> return $ Left AccountNotFound
        (Just toAccount) -> do
          case txType of
            Transactional -> performTransferTransactional fromAccount toAccount amount reference
            Eventual      -> performTransferEventual fromAccount toAccount amount reference

          -- TODO: put this into a domain service
          {-
          if not $ sameOwner fromAccount toAccount
            -- not same owner, can only transfer between giros and max 5000 amount
            then do
              if isSavings fromAccount || isSavings  toAccount
                then return $ Left $ InvalidAccountOperation "Transfer cannot happen with Savings account of different customers!"
                else if amount > 5000
                  then return $ Left $ InvalidAccountOperation "Transfer between different customers cannot exceed 5000€!"
                  else
                    case txType of
                      Transactional -> performTransferTransactional fromAccount toAccount amount reference
                      Eventual      -> performTransferEventual fromAccount toAccount amount reference
            -- same owner, anything goes, no restrictions
            else do
              case txType of
                      Transactional -> performTransferTransactional fromAccount toAccount amount reference
                      Eventual      -> performTransferEventual fromAccount toAccount amount reference
            -}

performTransferTransactional :: Account
                             -> Account
                             -> Double
                             -> T.Text
                             -> Application (Either Exception TXLineDTO)
performTransferTransactional fromAccount toAccount amount reference = do
  fromOwner <- runAggregate $ accountAggregate $ getOwner fromAccount
  mfc       <- runRepo $ customerRepo $ findCustomerById fromOwner
  case mfc of
    Nothing -> return $ Left CustomerNotFound
    (Just fromCustomer) -> do
      toOwner <- runAggregate $ accountAggregate $ getOwner toAccount
      mtc     <- runRepo $ customerRepo $ findCustomerById toOwner
      case mtc of
        Nothing -> return $ Left CustomerNotFound
        (Just toCustomer) -> do
          fromName <- runAggregate $ customerAggregate $ getName fromCustomer
          toName   <- runAggregate $ customerAggregate $ getName toCustomer
          fromIban <- runAggregate $ accountAggregate $ getIban fromAccount
          toIban   <- runAggregate $ accountAggregate $ getIban toAccount

          (_, retTo, _) <- runAggregate $ accountAggregate $ transferTo fromAccount toIban amount toName reference
          case retTo of
            (TransferToResult (Right fromTxLine)) -> do
              (_, retFrom, _) <- runAggregate $ accountAggregate $ receiveFrom toAccount fromIban amount fromName reference
              case retFrom of
                (ReceiveFromResult (Right _)) -> do
                  return $ Right $ txLineToDTO fromTxLine
                (ReceiveFromResult (Left err)) ->
                  return $ Left err
                _ ->
                  error "unexpected result"
            (TransferToResult (Left err)) ->
              return $ Left err
            _ ->
              error "unexpected result"

performTransferEventual :: Account
                        -> Account
                        -> Double
                        -> T.Text
                        -> Application (Either Exception TXLineDTO)
performTransferEventual fromAccount toAccount amount reference = do
  fromOwner <- runAggregate $ accountAggregate $ getOwner fromAccount
  mfc       <- runRepo $ customerRepo $ findCustomerById fromOwner
  case mfc of
    Nothing -> return $ Left CustomerNotFound
    (Just fromCustomer) -> do
      toOwner <- runAggregate $ accountAggregate $ getOwner toAccount
      mtc     <- runRepo $ customerRepo $ findCustomerById toOwner
      case mtc of
        Nothing -> return $ Left CustomerNotFound
        (Just toCustomer) -> do
          toName        <- runAggregate $ accountAggregate $ getName toCustomer
          (Iban toIban) <- runAggregate $ accountAggregate $ getIban toAccount

          (_, ret, _) <- runAggregate $ accountAggregate $ transferTo fromAccount toIban amount toName reference
          case ret of
            (Just (TransferToResult (Right txLine))) -> do
              (Iban fromIban) <- runAggregate $ accountAggregate $ getIban fromAccount

              let evtData = TransferSentEventData {
                  transferSentEventAmount            = amount
                , transferSentEventReference         = reference
                , transferSentEventSendingCustomer   = customerIdToText fromOwner
                , transferSentEventReceivingCustomer = customerIdToText toOwner
                , transferSentEventSendingAccount    = fromIban
                , transferSentEventReceivingAccount  = toIban
                }

              persistDomainEvent (DomainEvents.TransferSent evtData)
              return $ Right $ txLineToDTO txLine

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
    , accountDetailType    = T.pack $ show $ accType
    }

txLineToDTO :: TXLine -> TXLineDTO
txLineToDTO (TXLine a (Iban i) n ref t) = TXLineDTO
  { txLineIban      = i
  , txLineName      = n
  , txLineReference = ref
  , txLineAmount    = a
  , txLineTime      = t
  }
