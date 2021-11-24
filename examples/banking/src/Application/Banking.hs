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

import Infrastructure.Cache.AppCache 
import Infrastructure.DB.PgPool
import Infrastructure.DB.BankingDb as DB

-- TODO: transaction boundaries!
-- TODO: use cache (invalidate upon writes!)

data Exception
  = CustomerNotFound
  | AccountNotFound
  | InvalidAccountOperation T.Text

createCustomer :: AppCache
               -> PgPool
               -> T.Text
               -> IO T.Text
createCustomer _cache dbPool name = do
  custDomainId <- toText <$> nextRandom

  let cust = Customer custDomainId name
  _cid <- DB.insertCustomer dbPool cust
  return custDomainId

createAccount :: AppCache
              -> PgPool
              -> T.Text
              -> T.Text
              -> Double
              -> T.Text
              -> IO (Maybe Exception)
createAccount _cache dbPool owner iban balance t = do
  mc <- DB.customerById dbPool owner
  case mc of
    Nothing -> return $ Just CustomerNotFound
    (Just (Entity cid _)) -> do
      let acc = Account cid balance iban t
      _aid <- DB.insertAccount dbPool acc
      return Nothing

getAllCustomers :: AppCache
                -> PgPool 
                -> IO [CustomerDetailsDTO]
getAllCustomers _cache dbPool = do
  cs <- DB.allCustomers dbPool
  return $ map customerEntityToDetailsDTO cs

getCustomer :: AppCache
            -> PgPool
            -> T.Text
            -> IO (Either Exception CustomerDTO)
getCustomer _cache dbPool cIdStr = do
  mc <- DB.customerById dbPool cIdStr
  case mc of 
    Nothing -> return $ Left CustomerNotFound
    (Just c@(Entity cid _)) -> do
      as <- DB.accountsOfCustomer dbPool cid 
      return $ Right $ customerEntityToDTO c as

getAccount :: AppCache
           -> PgPool
           -> T.Text
           -> IO (Either Exception AccountDTO)
getAccount _cache dbPool iban = do
  ma <- DB.accountByIban dbPool iban
  case ma of 
    Nothing -> return $ Left AccountNotFound
    (Just a@(Entity aid _)) -> do
      txs <- DB.txLinesOfAccount dbPool aid
      return $ Right $ accountEntityToDTO a txs

deposit :: AppCache
        -> PgPool
        -> T.Text 
        -> Double 
        -> IO (Maybe Exception)
deposit _cache dbPool iban amount = do
  ma <- DB.accountByIban dbPool iban
  case ma of 
    Nothing -> return $ Just AccountNotFound
    (Just (Entity aid a)) -> do
      -- TODO: check Account Type! only allowed for Giro!

      now <- getCurrentTime
      let newBalance = amount + accountBalance a 
          newTxLine  = TXLine aid iban amount "Deposit" "Deposit" now

      txId <- DB.insertTXLine dbPool newTxLine
      print txId
      DB.updateAccountBalance dbPool aid newBalance
      return Nothing

withdraw :: AppCache
         -> PgPool
         -> T.Text 
         -> Double 
         -> IO (Maybe Exception)
withdraw _cache dbPool iban amount = do
  ma <- DB.accountByIban dbPool iban
  case ma of 
    Nothing -> return $ Just AccountNotFound
    (Just (Entity aid a)) -> do
      -- TODO: check Account Type! only allowed for Giro!
      let newBalance = accountBalance a - amount
      if newBalance < -1000
        then return $ Just $ InvalidAccountOperation "Cannot overdraw Giro account by more than -1000.0!"
        else do
          now <- getCurrentTime
          let newTxLine  = TXLine aid iban (-amount) "Deposit" "Deposit" now

          txId <- DB.insertTXLine dbPool newTxLine
          print txId

          DB.updateAccountBalance dbPool aid newBalance

          return Nothing

transfer :: AppCache 
         -> PgPool
         -> T.Text 
         -> T.Text 
         -> Double 
         -> T.Text 
         -> IO (Maybe Exception)
transfer _cache dbPool fromIban toIban amount reference = do
  mFrom <- DB.accountByIban dbPool fromIban
  case mFrom of 
    Nothing -> return $ Just AccountNotFound
    (Just (Entity fromAid fromAccount)) -> do
      mTo <- DB.accountByIban dbPool toIban
      case mTo of 
        Nothing -> return $ Just AccountNotFound
        (Just (Entity toAid toAccount)) -> do
          now <- getCurrentTime

          -- TODO: fetch user for name in TXLine
          let nameStr = "Transfer"

          -- TODO: if accounts belong to different customers, then check if giro and below 5000
          
          let newFromTxLine  = TXLine fromAid toIban (-amount) nameStr reference now
              newToTxLine    = TXLine toAid fromIban amount nameStr reference now

          _ <- DB.insertTXLine dbPool newFromTxLine
          _ <- DB.insertTXLine dbPool newToTxLine
          
          DB.updateAccountBalance dbPool fromAid (accountBalance fromAccount - amount)
          DB.updateAccountBalance dbPool toAid (accountBalance toAccount - amount)

          return Nothing

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
  , accountDetailType    = accountType a
  }
