{-# LANGUAGE OverloadedStrings          #-}
module Application.Banking
  ( Exception (..)
  , createCustomer
  , createAccount
  , getAllCustomers
  , getCustomer
  , getAccount
  , deposit
  , withdraw
  , transfer
  ) where

import qualified Data.Text as T
import Data.Time.Clock
import Data.UUID.V4 (nextRandom)
import Data.UUID

import Application.DTO

import Database.Persist.Postgresql
import Infrastructure.Cache.AppCache 
import Infrastructure.DB.Banking as DB

data Exception
  = CustomerNotFound
  | AccountNotFound
  | InvalidAccountOperation T.Text
  deriving Show

createCustomer :: AppCache
               -> T.Text
               -> SqlBackend
               -> IO T.Text
createCustomer _cache name conn = do
  custDomainId <- toText <$> nextRandom

  let cust = Customer custDomainId name
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
  mc <- DB.customerById owner conn
  case mc of
    Nothing -> return $ Just CustomerNotFound
    (Just (Entity cid _)) -> do
      let at = read (T.unpack t) :: DB.AccountType
      let acc = Account cid balance iban at
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
  mc <- DB.customerById cIdStr conn
  case mc of 
    Nothing -> return $ Left CustomerNotFound
    (Just c@(Entity cid _)) -> do
      as <- DB.accountsOfCustomer cid conn
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
          let newBalance = amount + accountBalance a 
              newTxLine  = TXLine aid iban amount "Deposit" "Deposit" now

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
          let newBalance = accountBalance a - amount
          if newBalance < -1000
            then return $ Left $ InvalidAccountOperation "Cannot overdraw Giro account by more than -1000.0!"
            else do
              now <- getCurrentTime
              let newTxLine  = TXLine aid iban (-amount) "Deposit" "Deposit" now

              txId <- DB.insertTXLine newTxLine conn
              print txId

              DB.updateAccountBalance aid newBalance conn

              return $ Right $ txLineToDTO (Entity txId newTxLine)

transfer :: AppCache 
         -> T.Text 
         -> T.Text 
         -> Double 
         -> T.Text 
         -> SqlBackend
         -> IO (Either Exception TXLineDTO)
transfer _cache fromIban toIban amount reference conn = do
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
                  else performTransfer fromAid fromAccount toAid toAccount amount reference conn
            -- same owner, anything goes, no restrictions
            else do
              performTransfer fromAid fromAccount toAid toAccount amount reference conn

performTransfer :: AccountId
                -> Account
                -> AccountId
                -> Account
                -> Double
                -> T.Text
                -> SqlBackend
                -> IO (Either Exception TXLineDTO)
performTransfer fromAid fromAccount toAid toAccount amount reference conn = do
  mfc <- DB.customerByCustomerId (accountOwner fromAccount) conn
  case mfc of 
    Nothing -> return $ Left CustomerNotFound
    (Just fromCustomer) -> do
      mtc <- DB.customerByCustomerId (accountOwner toAccount) conn
      case mtc of 
        Nothing -> return $ Left CustomerNotFound
        (Just toCustomer) -> do
          let check = checkAccountOverdraft fromAccount amount
          case check of 
            (Just err) -> return $ Left err
            _ -> do
              let fromName = customerName fromCustomer
                  toName   = customerName toCustomer

              now <- getCurrentTime

              let newFromTxLine  = TXLine fromAid (accountIban toAccount) (-amount) toName reference now
                  newToTxLine    = TXLine toAid (accountIban fromAccount) amount fromName reference now

              fromTxId <- DB.insertTXLine newFromTxLine conn
              _ <- DB.insertTXLine newToTxLine conn
              
              DB.updateAccountBalance fromAid (accountBalance fromAccount - amount) conn
              DB.updateAccountBalance toAid (accountBalance toAccount + amount) conn

              return $ Right $ txLineToDTO (Entity fromTxId newFromTxLine)

checkAccountOverdraft :: Account -> Double -> Maybe Exception
checkAccountOverdraft a amount
  | isSavings a && accountBalance a - amount < 0  = Just $ InvalidAccountOperation "Savings Account cannot have negative balance!"
  | isGiro a && accountBalance a - amount < -1000 = Just $ InvalidAccountOperation "Cannot overdraw Giro account by more than -1000.0!"
  | otherwise = Nothing 

isSavings :: Account -> Bool
isSavings a = DB.Savings == accountType a

isGiro :: Account -> Bool
isGiro a = DB.Giro == accountType a

sameOwner :: Account -> Account -> Bool
sameOwner a1 a2 = accountOwner a1 == accountOwner a2

accountEntityToDTO :: Entity Account -> [Entity TXLine] -> AccountDTO 
accountEntityToDTO a txs = AccountDTO 
  { accountDetails = accountEntityToDetailsDTO a
  , accountTXLines = map txLineToDTO txs
  }

txLineToDTO :: Entity TXLine -> TXLineDTO
txLineToDTO (Entity _ t) = TXLineDTO
  { txLineIban      = tXLineIban t
  , txLineName      = tXLineName t
  , txLineReference = tXLineReference t
  , txLineAmount    = tXLineAmount t
  , txLineTime      = tXLineTime t
  }

customerEntityToDetailsDTO :: Entity Customer -> CustomerDetailsDTO
customerEntityToDetailsDTO (Entity _ c) = CustomerDetailsDTO 
  { customerDetailsId   = customerDomainId c
  , customerDetailsName = customerName c
  }

customerEntityToDTO :: Entity Customer -> [Entity Account] -> CustomerDTO
customerEntityToDTO c as = CustomerDTO 
  { customerDetails        = customerEntityToDetailsDTO c
  , customerAccountDetails = map accountEntityToDetailsDTO as
  }

accountEntityToDetailsDTO :: Entity Account -> AccountDetailsDTO
accountEntityToDetailsDTO (Entity _ a) = AccountDetailsDTO 
  { accountDetailIban    = accountIban a
  , accountDetailBalance = accountBalance a
  , accountDetailType    = T.pack $ show $ accountType a
  }
