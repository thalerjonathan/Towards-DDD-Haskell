{-# LANGUAGE OverloadedStrings #-}
module Application.BankingAnemic
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

import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL

import           Data.Aeson.Text
import           Data.Time.Clock
import           Data.UUID
import           Data.UUID.V4                  (nextRandom)

import           Application.DTO
import           Application.DomainEvents
import           Application.Exceptions

import           Database.Persist.Postgresql
import           Infrastructure.Cache.AppCache
import           Infrastructure.DB.Banking     as DB


createCustomer :: AppCache
               -> T.Text
               -> SqlBackend
               -> IO T.Text
createCustomer _cache name conn = do
  custDomainId <- toText <$> nextRandom
  let cust = CustomerEntity custDomainId name
  _cid <- DB.insertCustomer cust conn
  return custDomainId

createAccount :: AppCache
              -> T.Text
              -> T.Text
              -> Double
              -> T.Text
              -> SqlBackend
              -> IO (Maybe Exception)
createAccount _cache owner iban balance t conn = do
  mc <- DB.customerByDomainId owner conn
  case mc of
    Nothing -> return $ Just CustomerNotFound
    (Just (Entity _ _)) -> do
      let at = read (T.unpack t) :: DB.AccountEntityType
      let acc = AccountEntity owner balance iban at
      _aid <- DB.insertAccount acc conn
      return Nothing

getAllCustomers :: AppCache
                -> SqlBackend
                -> IO [CustomerDetailsDTO]
getAllCustomers _cache conn = do
  cs <- DB.allCustomers conn
  return $ map customerEntityToDetailsDTO cs

getCustomer :: AppCache
            -> T.Text
            -> SqlBackend
            -> IO (Either Exception CustomerDTO)
getCustomer _cache cIdStr conn = do
  mc <- DB.customerByDomainId cIdStr conn
  case mc of
    Nothing -> return $ Left CustomerNotFound
    (Just c@(Entity _ _)) -> do
      as <- DB.accountsOfCustomer cIdStr conn
      return $ Right $ customerEntityToDTO c as

getAccount :: AppCache
           -> T.Text
           -> SqlBackend
           -> IO (Either Exception AccountDTO)
getAccount _cache iban conn = do
  ma <- DB.accountByIban iban conn
  case ma of
    Nothing -> return $ Left AccountNotFound
    (Just a@(Entity aid _)) -> do
      txs <- DB.txLinesOfAccount aid conn
      return $ Right $ accountEntityToDTO a txs

deposit :: AppCache
        -> T.Text
        -> Double
        -> SqlBackend
        -> IO (Either Exception TXLineDTO)
deposit _cache iban amount conn = do
  ma <- DB.accountByIban iban conn
  case ma of
    Nothing -> return $ Left AccountNotFound
    (Just (Entity aid a)) -> do
      if isSavings a
        then return $ Left $ InvalidAccountOperation "Cannot deposit into Savings account!"
        else do
          now <- getCurrentTime
          let newBalance = amount + accountEntityBalance a
              newTxLine  = TxLineEntity aid iban amount "Deposit" "Deposit" now

          txId <- DB.insertTXLine newTxLine conn
          print txId
          DB.updateAccountBalance aid newBalance conn
          return $ Right $ txLineToDTO (Entity txId newTxLine)

withdraw :: AppCache
         -> T.Text
         -> Double
         -> SqlBackend
         -> IO (Either Exception TXLineDTO)
withdraw _cache iban amount conn = do
  ma <- DB.accountByIban iban conn
  case ma of
    Nothing -> return $ Left AccountNotFound
    (Just (Entity aid a)) -> do
      if isSavings a
        then return $ Left $ InvalidAccountOperation "Cannot withdraw from Savings account!"
        else do
          let newBalance = accountEntityBalance a - amount
          if newBalance < -1000
            then return $ Left $ InvalidAccountOperation "Cannot overdraw Giro account by more than -1000.0!"
            else do
              now <- getCurrentTime
              let newTxLine  = TxLineEntity aid iban (-amount) "Deposit" "Deposit" now

              txId <- DB.insertTXLine newTxLine conn
              print txId

              DB.updateAccountBalance aid newBalance conn

              return $ Right $ txLineToDTO (Entity txId newTxLine)

data TransferType = Transactional | Eventual

transferEventual :: AppCache
                 -> T.Text
                 -> T.Text
                 -> Double
                 -> T.Text
                 -> SqlBackend
                 -> IO (Either Exception TXLineDTO)
transferEventual _cache fromIban toIban amount reference conn =
  checkAndPerformTransfer fromIban toIban amount reference conn Eventual

transferTransactional :: AppCache
                      -> T.Text
                      -> T.Text
                      -> Double
                      -> T.Text
                      -> SqlBackend
                      -> IO (Either Exception TXLineDTO)
transferTransactional _cache fromIban toIban amount reference conn =
  checkAndPerformTransfer fromIban toIban amount reference conn Transactional

checkAndPerformTransfer :: T.Text
                        -> T.Text
                        -> Double
                        -> T.Text
                        -> SqlBackend
                        -> TransferType
                        -> IO (Either Exception TXLineDTO)
checkAndPerformTransfer fromIban toIban amount reference conn txType = do
  mFrom <- DB.accountByIban fromIban conn
  case mFrom of
    Nothing -> return $ Left AccountNotFound
    (Just (Entity fromAid fromAccount)) -> do
      mTo <- DB.accountByIban toIban conn
      case mTo of
        Nothing -> return $ Left AccountNotFound
        (Just (Entity toAid toAccount)) -> do
          if not $ sameOwner fromAccount toAccount
            -- not same owner, can only transfer between giros and max 5000 amount
            then do
              if isSavings fromAccount || isSavings  toAccount
                then return $ Left $ InvalidAccountOperation "Transfer cannot happen with Savings account of different customers!"
                else if amount > 5000
                  then return $ Left $ InvalidAccountOperation "Transfer between different customers cannot exceed 5000€!"
                  else
                    case txType of
                      Transactional -> performTransferTransactional fromAid fromAccount toAid toAccount amount reference conn
                      Eventual      -> performTransferEventual fromAid fromAccount toAccount amount reference conn
            -- same owner, anything goes, no restrictions
            else do
              case txType of
                      Transactional -> performTransferTransactional fromAid fromAccount toAid toAccount amount reference conn
                      Eventual      -> performTransferEventual fromAid fromAccount toAccount amount reference conn

performTransferTransactional :: AccountEntityId
                             -> AccountEntity
                             -> AccountEntityId
                             -> AccountEntity
                             -> Double
                             -> T.Text
                             -> SqlBackend
                             -> IO (Either Exception TXLineDTO)
performTransferTransactional fromAid fromAccount toAid toAccount amount reference conn = do
  mfc <- DB.customerByDomainId (accountEntityOwner fromAccount) conn
  case mfc of
    Nothing -> return $ Left CustomerNotFound
    (Just (Entity _ fromCustomer)) -> do
      mtc <- DB.customerByDomainId (accountEntityOwner toAccount) conn
      case mtc of
        Nothing -> return $ Left CustomerNotFound
        (Just (Entity _ toCustomer)) -> do
          let check = checkAccountOverdraft fromAccount amount
          case check of
            (Just err) -> return $ Left err
            _ -> do
              let fromName = customerEntityName fromCustomer
                  toName   = customerEntityName toCustomer

              now <- getCurrentTime

              let newFromTxLine  = TxLineEntity fromAid ( accountEntityIban toAccount) (-amount) toName reference now
                  newToTxLine    = TxLineEntity toAid ( accountEntityIban fromAccount) amount fromName reference now

              fromTxId <- DB.insertTXLine newFromTxLine conn
              _ <- DB.insertTXLine newToTxLine conn

              DB.updateAccountBalance fromAid (accountEntityBalance fromAccount - amount) conn
              DB.updateAccountBalance toAid (accountEntityBalance toAccount + amount) conn

              return $ Right $ txLineToDTO (Entity fromTxId newFromTxLine)

performTransferEventual :: AccountEntityId
                        -> AccountEntity
                        -> AccountEntity
                        -> Double
                        -> T.Text
                        -> SqlBackend
                        -> IO (Either Exception TXLineDTO)
performTransferEventual fromAid fromAccount toAccount amount reference conn = do
  mfc <- DB.customerByDomainId (accountEntityOwner fromAccount) conn
  case mfc of
    Nothing -> return $ Left CustomerNotFound
    (Just (Entity _ fromCustomer)) -> do
      mtc <- DB.customerByDomainId (accountEntityOwner toAccount) conn
      case mtc of
        Nothing -> return $ Left CustomerNotFound
        (Just (Entity _ toCustomer)) -> do
          let check = checkAccountOverdraft fromAccount amount
          case check of
            (Just err) -> return $ Left err
            _ -> do
              let toName = customerEntityName toCustomer

              now <- getCurrentTime

              let newFromTxLine  = TxLineEntity fromAid (accountEntityIban toAccount) (-amount) toName reference now
              let evt = TransferSentEvent {
                  transferSentEventAmount            = amount
                , transferSentEventReference         = reference
                , transferSentEventSendingCustomer   = customerEntityDomainId fromCustomer
                , transferSentEventReceivingCustomer = customerEntityDomainId toCustomer
                , transferSentEventSendingAccount    = accountEntityIban fromAccount
                , transferSentEventReceivingAccount  = accountEntityIban toAccount
                }

              let payload = TL.toStrict $ encodeToLazyText evt
              let pe = PersistedEventEntity now "TransferSent" False False "" payload

              fromTxId <- DB.insertTXLine newFromTxLine conn

              DB.updateAccountBalance fromAid (accountEntityBalance fromAccount - amount) conn
              _ <- DB.insertEvent pe conn

              return $ Right $ txLineToDTO (Entity fromTxId newFromTxLine)

processDomainEvent :: DomainEvent
                   -> SqlBackend
                   -> IO ()
processDomainEvent (TransferSent evt)   = transferSent evt
processDomainEvent (TransferFailed evt) = transferFailed evt

transferSent :: TransferSentEvent
             -> SqlBackend
             -> IO ()
transferSent evt conn = do
  let fromIban  = transferSentEventSendingAccount evt
      toIban    = transferSentEventReceivingAccount evt
      amount    = transferSentEventAmount evt
      reference = transferSentEventReference evt

  mFrom <- DB.accountByIban fromIban conn
  case mFrom of
    Nothing -> transferSentFailed evt "Could not find sending Account"
    (Just (Entity _ fromAccount)) -> do
      mTo <- DB.accountByIban toIban conn
      case mTo of
        Nothing -> transferSentFailed evt "Could not find receiving Account"
        (Just (Entity toAid toAccount)) -> do
          mfc <- DB.customerByDomainId (accountEntityOwner fromAccount) conn
          case mfc of
            Nothing -> transferSentFailed evt "Could not find sending customer"
            (Just (Entity _ fromCustomer)) -> do
              mtc <- DB.customerByDomainId (accountEntityOwner toAccount) conn
              case mtc of
                Nothing -> transferSentFailed evt "Could not find receiving customer"
                (Just _) -> do
                  let fromName = customerEntityName fromCustomer

                  now <- getCurrentTime

                  let newToTxLine = TxLineEntity toAid ( accountEntityIban fromAccount) amount fromName reference now

                  _ <- DB.insertTXLine newToTxLine conn

                  DB.updateAccountBalance toAid (accountEntityBalance toAccount + amount) conn

                  return ()
  where
    transferSentFailed :: TransferSentEvent -> T.Text -> IO ()
    transferSentFailed evtSent err = do
      let evtFailed = TransferFailedEvent {
          transferFailedEventError             = err
        , transferFailedEventAmount            = transferSentEventAmount evtSent
        , transferFailedEventReference         = transferSentEventReference evtSent
        , transferFailedEventSendingCustomer   = transferSentEventSendingCustomer evtSent
        , transferFailedEventReceivingCustomer = transferSentEventReceivingCustomer evtSent
        , transferFailedEventSendingAccount    = transferSentEventSendingAccount evtSent
        , transferFailedEventReceivingAccount  = transferSentEventReceivingAccount evtSent
        }

      now <- getCurrentTime

      let payload = TL.toStrict $ encodeToLazyText evtFailed
      let pe = PersistedEventEntity now "TransferFailed" False False "" payload

      _ <- DB.insertEvent pe conn

      return ()


transferFailed :: TransferFailedEvent
               -> SqlBackend
               -> IO ()
transferFailed evt conn = do
  let fromIban  = transferFailedEventSendingAccount evt
      toIban    = transferFailedEventReceivingAccount evt
      amount    = transferFailedEventAmount evt
      reference = transferFailedEventReference evt

  mFrom <- DB.accountByIban fromIban conn
  case mFrom of
    Nothing -> return () -- ignore errors, what should we do?
    (Just (Entity fromAid fromAccount)) -> do
      mTo <- DB.accountByIban toIban conn
      case mTo of
        Nothing -> return () -- ignore errors, what should we do?
        (Just (Entity _ toAccount)) -> do
          mfc <- DB.customerByDomainId (accountEntityOwner fromAccount) conn
          case mfc of
            Nothing -> return () -- ignore errors, what should we do?
            (Just _) -> do
              mtc <- DB.customerByDomainId (accountEntityOwner toAccount) conn
              case mtc of
                Nothing -> return () -- ignore errors, what should we do?
                (Just (Entity _ toCustomer)) -> do
                  let toName = customerEntityName toCustomer

                  now <- getCurrentTime

                  let newFromTxLine = TxLineEntity fromAid ( accountEntityIban fromAccount) amount toName ("Transfer failed: " <> reference) now

                  _ <- DB.insertTXLine newFromTxLine conn

                  DB.updateAccountBalance fromAid (accountEntityBalance toAccount + amount) conn

                  return ()
                  
checkAccountOverdraft :: AccountEntity -> Double -> Maybe Exception
checkAccountOverdraft a amount
  | isSavings a && accountEntityBalance a - amount < 0  = Just $ InvalidAccountOperation "Savings Account cannot have negative balance!"
  | isGiro a && accountEntityBalance a - amount < -1000 = Just $ InvalidAccountOperation "Cannot overdraw Giro account by more than -1000.0!"
  | otherwise = Nothing

isSavings :: AccountEntity -> Bool
isSavings a = DB.Savings == accountEntityType a

isGiro :: AccountEntity -> Bool
isGiro a = DB.Giro == accountEntityType a

sameOwner :: AccountEntity -> AccountEntity -> Bool
sameOwner a1 a2 = accountEntityOwner a1 == accountEntityOwner a2

accountEntityToDTO :: Entity AccountEntity -> [Entity TxLineEntity] -> AccountDTO
accountEntityToDTO a txs = AccountDTO
  { accountDetails = accountEntityToDetailsDTO a
  , accountTXLines = map txLineToDTO txs
  }

txLineToDTO :: Entity TxLineEntity -> TXLineDTO
txLineToDTO (Entity _ t) = TXLineDTO
  { txLineIban      = txLineEntityIban t
  , txLineName      = txLineEntityName t
  , txLineReference = txLineEntityReference t
  , txLineAmount    = txLineEntityAmount t
  , txLineTime      = txLineEntityTime t
  }

customerEntityToDetailsDTO :: Entity CustomerEntity -> CustomerDetailsDTO
customerEntityToDetailsDTO (Entity _ c) = CustomerDetailsDTO
  { customerDetailsId   = customerEntityDomainId c
  , customerDetailsName = customerEntityName c
  }

customerEntityToDTO :: Entity CustomerEntity -> [Entity AccountEntity] -> CustomerDTO
customerEntityToDTO c as = CustomerDTO
  { customerDetails        = customerEntityToDetailsDTO c
  , customerAccountDetails = map accountEntityToDetailsDTO as
  }

accountEntityToDetailsDTO :: Entity AccountEntity -> AccountDetailsDTO
accountEntityToDetailsDTO (Entity _ a) = AccountDetailsDTO
  { accountDetailIban    = accountEntityIban a
  , accountDetailBalance = accountEntityBalance a
  , accountDetailType    = T.pack $ show $ accountEntityType a
  }
