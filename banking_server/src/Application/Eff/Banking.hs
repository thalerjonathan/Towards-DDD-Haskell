{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Application.Eff.Banking
  ( Exception (..)
  {-
  , createCustomer
  , createAccount
  -}
  , getAllCustomers
  , getCustomer
    {-
  , getAccount
  , deposit
  , withdraw
  , transferTransactional
  , transferEventual
  , processDomainEvent
  -}
  ) where

import           Application.DTO
import           Application.Eff.Layer
import           Application.Exceptions
import           Control.Monad.Except
import qualified Data.Text                      as T
import           Domain.Eff.Account
import           Domain.Eff.Account.Repository
import           Domain.Eff.Customer
import           Domain.Eff.Customer.Repository
import           Domain.Types
{-

import qualified Data.Text.Lazy              as TL


import           Application.Events

import           Data.Aeson.Text             as Aeson
import           Data.Time.Clock
import           Data.UUID
import           Data.UUID.V4                (nextRandom)

data TransferType = Transactional | Eventual

-- NOTE: this approach is built on https://hackage.haskell.org/package/effectful
-}

--type Application          = ApplicationLayer
--type ApplicationExcept ex m = ExceptT ex (ApplicationLayer m)

{--
createCustomer :: T.Text
               -> SqlBackend
               -> Application T.Text
createCustomer name conn = do
  custDomainId <- toText <$> nextRandom
  -- let cust = CustomerEntity custDomainId name
  -- _cid <- DB.insertCustomer cust conn
  return custDomainId

createAccount :: T.Text
              -> T.Text
              -> Double
              -> T.Text
              -> SqlBackend
              -> ApplicationExcept Exception ()
createAccount owner iban balance t conn = do
  -- _ <- tryMaybe (liftIO $ DB.customerByDomainId owner conn) CustomerNotFound
  -- let at = read (T.unpack t) :: DB.AccountEntityType
  -- let acc = AccountEntity owner balance iban at
  -- _aid <- liftIO $ DB.insertAccount acc conn
  return ()
--}

getAllCustomers :: (ApplicationLayer m, CustomerRepo m)
                => m [CustomerDetailsDTO]
getAllCustomers = do
  logging Debug "getAllCustomers"
  cs <- allCustomers
  return $ map customerToDetailsDTO cs

getCustomer :: (ApplicationLayer m, CustomerRepo m, AccountRepo m, MonadError Exception m)
            => T.Text
            -> m CustomerDTO
getCustomer cIdStr = do
  logging Debug "getCustomer"
  let cid = customerIdFromTextUnsafe cIdStr
  c  <- tryMaybe' (findCustomerById cid) CustomerNotFound
  as <- findAccountsForOwner cid
  return $ customerToDTO c as

tryMaybe' :: (MonadError ex m) => m (Maybe a) -> ex -> m a
tryMaybe' act ex = do
  ret <- act
  case ret of
    Nothing  -> throwError ex
    (Just a) -> return a

{-
getAccount :: T.Text
           -> SqlBackend
           -> ApplicationExcept Exception AccountDTO
getAccount iban conn = do
  -- a@(Entity aid _) <- tryMaybe (DB.accountByIban iban conn) AccountNotFound
  -- txs <- liftIO $ DB.txLinesOfAccount aid conn
  -- return $ accountEntityToDTO a txs
  undefined

deposit :: T.Text
        -> Double
        -> SqlBackend
        -> ApplicationExcept Exception TXLineDTO
deposit iban amount conn = do
  (Entity aid a) <- tryMaybe (DB.accountByIban iban conn) AccountNotFound
  if isSavings a
    then throwError $ InvalidAccountOperation "Cannot deposit into Savings account!"
    else do
      now <- liftIO getCurrentTime
      let newBalance = amount + accountEntityBalance a
          newTxLine  = TxLineEntity aid iban amount "Deposit" "Deposit" now

      txId <- liftIO $ DB.insertTXLine newTxLine conn
      liftIO $ DB.updateAccountBalance aid newBalance conn
      return $ txLineToDTO (Entity txId newTxLine)

withdraw :: T.Text
         -> Double
         -> SqlBackend
         -> ApplicationExcept Exception TXLineDTO
withdraw iban amount conn = do
  (Entity aid a) <- tryMaybe (DB.accountByIban iban conn) AccountNotFound
  if isSavings a
    then throwError $ InvalidAccountOperation "Cannot withdraw from Savings account!"
    else do
      guardWith (checkAccountOverdraft a amount)

      now <- liftIO getCurrentTime
      let newTxLine  = TxLineEntity aid iban (-amount) "Deposit" "Deposit" now
          newBalance = accountEntityBalance a - amount

      txId <- liftIO $  DB.insertTXLine newTxLine conn
      liftIO $ DB.updateAccountBalance aid newBalance conn
      return $ txLineToDTO (Entity txId newTxLine)

transferEventual :: T.Text
                 -> T.Text
                 -> Double
                 -> T.Text
                 -> SqlBackend
                 -> ApplicationExcept Exception TXLineDTO
transferEventual fromIban toIban amount reference conn =
  checkAndPerformTransfer fromIban toIban amount reference conn Eventual

transferTransactional :: T.Text
                      -> T.Text
                      -> Double
                      -> T.Text
                      -> SqlBackend
                      -> ApplicationExcept Exception TXLineDTO
transferTransactional fromIban toIban amount reference conn =
  checkAndPerformTransfer fromIban toIban amount reference conn Transactional

checkAndPerformTransfer :: T.Text
                        -> T.Text
                        -> Double
                        -> T.Text
                        -> SqlBackend
                        -> TransferType
                        -> ApplicationExcept Exception TXLineDTO
checkAndPerformTransfer fromIban toIban amount reference conn txType = do
  (Entity fromAid fromAccount) <- tryMaybe (DB.accountByIban fromIban conn) AccountNotFound
  (Entity toAid toAccount)     <- tryMaybe (DB.accountByIban toIban conn) AccountNotFound
  if not $ sameOwner fromAccount toAccount
    -- not same owner, can only transfer between giros and max 5000 amount
    then do
      if isSavings fromAccount || isSavings  toAccount
        then throwError $ InvalidAccountOperation "Transfer cannot happen with Savings account of different customers!"
        else if amount > 5000
          then throwError $ InvalidAccountOperation "Transfer between different customers cannot exceed 5000!"
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
                             -> ApplicationExcept Exception TXLineDTO
performTransferTransactional fromAid fromAccount toAid toAccount amount reference conn = do
  (Entity _ fromCustomer) <- tryMaybe (DB.customerByDomainId (accountEntityOwner fromAccount) conn) CustomerNotFound
  (Entity _ toCustomer)   <- tryMaybe (DB.customerByDomainId (accountEntityOwner toAccount) conn) CustomerNotFound

  guardWith (checkAccountOverdraft fromAccount amount)

  let fromName = customerEntityName fromCustomer
      toName   = customerEntityName toCustomer

  now <- liftIO getCurrentTime

  let newFromTxLine  = TxLineEntity fromAid ( accountEntityIban toAccount) (-amount) toName reference now
      newToTxLine    = TxLineEntity toAid ( accountEntityIban fromAccount) amount fromName reference now

  fromTxId <- liftIO $ DB.insertTXLine newFromTxLine conn
  _ <- liftIO $ DB.insertTXLine newToTxLine conn

  liftIO $ DB.updateAccountBalance fromAid (accountEntityBalance fromAccount - amount) conn
  liftIO $ DB.updateAccountBalance toAid (accountEntityBalance toAccount + amount) conn

  return $ txLineToDTO (Entity fromTxId newFromTxLine)

performTransferEventual :: AccountEntityId
                        -> AccountEntity
                        -> AccountEntity
                        -> Double
                        -> T.Text
                        -> SqlBackend
                        -> ApplicationExcept Exception TXLineDTO
performTransferEventual fromAid fromAccount toAccount amount reference conn = do
  (Entity _ fromCustomer) <- tryMaybe (DB.customerByDomainId (accountEntityOwner fromAccount) conn) CustomerNotFound
  (Entity _ toCustomer)   <- tryMaybe (DB.customerByDomainId (accountEntityOwner toAccount) conn) CustomerNotFound

  guardWith (checkAccountOverdraft fromAccount amount)

  let toName = customerEntityName toCustomer

  now <- liftIO getCurrentTime

  let newFromTxLine  = TxLineEntity fromAid (accountEntityIban toAccount) (-amount) toName reference now
  let evt = TransferSentEventData {
      transferSentEventAmount            = amount
    , transferSentEventReference         = reference
    , transferSentEventSendingCustomer   = customerEntityDomainId fromCustomer
    , transferSentEventReceivingCustomer = customerEntityDomainId toCustomer
    , transferSentEventSendingAccount    = accountEntityIban fromAccount
    , transferSentEventReceivingAccount  = accountEntityIban toAccount
    }

  let payload = TL.toStrict $ encodeToLazyText evt
  let pe = PersistedEventEntity now "TransferSent" False False "" payload

  fromTxId <- liftIO $ DB.insertTXLine newFromTxLine conn

  liftIO $ DB.updateAccountBalance fromAid (accountEntityBalance fromAccount - amount) conn
  _ <- liftIO $ DB.insertEvent pe conn

  return $ txLineToDTO (Entity fromTxId newFromTxLine)

processDomainEvent :: DomainEvent
                   -> SqlBackend
                   -> Application ()
processDomainEvent (TransferSent evt)  conn = void (runExceptT (transferSent evt conn))
processDomainEvent (TransferFailed evt) conn = void (runExceptT (transferFailed evt conn))

transferSent :: TransferSentEventData
             -> SqlBackend
             -> ApplicationExcept () ()
transferSent evt conn = do
    let fromIban  = transferSentEventSendingAccount evt
        toIban    = transferSentEventReceivingAccount evt
        amount    = transferSentEventAmount evt
        reference = transferSentEventReference evt

    (Entity _ fromAccount) <- tryMaybeM
                                (DB.accountByIban fromIban conn)
                                (transferSentFailed evt "Could not find sending Account")

    (Entity toAid toAccount) <- tryMaybeM
                                  (DB.accountByIban toIban conn)
                                  (transferSentFailed evt "Could not find receiving Account")

    (Entity _ fromCustomer) <- tryMaybeM
                                (DB.customerByDomainId (accountEntityOwner fromAccount) conn)
                                (transferSentFailed evt "Could not find sending customer")

    _ <- tryMaybeM
          (DB.customerByDomainId (accountEntityOwner toAccount) conn)
          (transferSentFailed evt "Could not find receiving customer")

    now <- liftIO getCurrentTime

    let fromName    = customerEntityName fromCustomer
        newToTxLine = TxLineEntity toAid ( accountEntityIban fromAccount) amount fromName reference now

    _ <- liftIO $ DB.insertTXLine newToTxLine conn
    liftIO $ DB.updateAccountBalance toAid (accountEntityBalance toAccount + amount) conn
  where
    transferSentFailed :: TransferSentEventData -> T.Text -> Application ()
    transferSentFailed evtSent err = do
      let evtFailed = TransferFailedEventData {
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

transferFailed :: TransferFailedEventData
               -> SqlBackend
               -> ApplicationExcept () ()
transferFailed evt conn = do
  let fromIban  = transferFailedEventSendingAccount evt
      toIban    = transferFailedEventReceivingAccount evt
      amount    = transferFailedEventAmount evt
      reference = transferFailedEventReference evt

  (Entity fromAid fromAccount) <- tryMaybeM
                                    (DB.accountByIban fromIban conn)
                                    (putStrLn "Processing TransferFailed event failed: could not find sending Account!")

  (Entity _ toAccount) <- tryMaybeM
                            (DB.accountByIban toIban conn)
                            (putStrLn "Processing TransferFailed event failed: could not find receiving Account!")

  _ <- tryMaybeM
        (DB.customerByDomainId (accountEntityOwner fromAccount) conn)
        (putStrLn "Processing TransferFailed event failed: could not find sending Customer!")

  (Entity _ toCustomer) <- tryMaybeM
                            (DB.customerByDomainId (accountEntityOwner toAccount) conn)
                            (putStrLn "Processing TransferFailed event failed: could not find receiving Customer!")
  now <- liftIO getCurrentTime

  let toName        = customerEntityName toCustomer
      newFromTxLine = TxLineEntity fromAid ( accountEntityIban fromAccount) amount toName ("Transfer failed: " <> reference) now

  _ <- liftIO $ DB.insertTXLine newFromTxLine conn
  liftIO $ DB.updateAccountBalance fromAid (accountEntityBalance toAccount + amount) conn

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
-}

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
accountToDetailsDTO (Account (Iban aIban) aBalance aType) = AccountDetailsDTO
  { accountDetailIban    = aIban
  , accountDetailBalance = aBalance
  , accountDetailType    = T.pack $ show aType
  }
