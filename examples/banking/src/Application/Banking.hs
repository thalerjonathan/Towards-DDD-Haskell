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
import Infrastructure.DB.BankingDb as DB

-- TODO: transaction boundaries!
-- TODO: use cache (invalidate upon writes!)

data Exception
  = CustomerNotFound
  | AccountNotFound
  | InvalidAccountOperation T.Text

createCustomer :: AppCache
               -> SqlBackend
               -> T.Text
               -> IO T.Text
createCustomer _cache conn name = do
  custDomainId <- toText <$> nextRandom

  let cust = Customer custDomainId name
  _cid <- DB.insertCustomer conn cust
  return custDomainId

createAccount :: AppCache
              -> SqlBackend
              -> T.Text
              -> T.Text
              -> Double
              -> T.Text
              -> IO (Maybe Exception)
createAccount _cache conn owner iban balance t = do
  mc <- DB.customerById conn owner
  case mc of
    Nothing -> return $ Just CustomerNotFound
    (Just (Entity cid _)) -> do
      let acc = Account cid balance iban t
      _aid <- DB.insertAccount conn acc
      return Nothing

getAllCustomers :: AppCache
                -> SqlBackend 
                -> IO [CustomerDetailsDTO]
getAllCustomers _cache conn = do
  cs <- DB.allCustomers conn
  return $ map customerEntityToDetailsDTO cs

getCustomer :: AppCache
            -> SqlBackend
            -> T.Text
            -> IO (Either Exception CustomerDTO)
getCustomer _cache conn cIdStr = do
  mc <- DB.customerById conn cIdStr
  case mc of 
    Nothing -> return $ Left CustomerNotFound
    (Just c@(Entity cid _)) -> do
      as <- DB.accountsOfCustomer conn cid 
      return $ Right $ customerEntityToDTO c as

getAccount :: AppCache
           -> SqlBackend
           -> T.Text
           -> IO (Either Exception AccountDTO)
getAccount _cache conn iban = do
  ma <- DB.accountByIban conn iban
  case ma of 
    Nothing -> return $ Left AccountNotFound
    (Just a@(Entity aid _)) -> do
      txs <- DB.txLinesOfAccount conn aid
      return $ Right $ accountEntityToDTO a txs

deposit :: AppCache
        -> SqlBackend
        -> T.Text 
        -> Double 
        -> IO (Maybe Exception)
deposit _cache conn iban amount = do
  ma <- DB.accountByIban conn iban
  case ma of 
    Nothing -> return $ Just AccountNotFound
    (Just (Entity aid a)) -> do
      -- TODO: check Account Type! only allowed for Giro!

      now <- getCurrentTime
      let newBalance = amount + accountBalance a 
          newTxLine  = TXLine aid iban amount "Deposit" "Deposit" now

      txId <- DB.insertTXLine conn newTxLine
      print txId
      DB.updateAccountBalance conn aid newBalance
      return Nothing

withdraw :: AppCache
         -> SqlBackend
         -> T.Text 
         -> Double 
         -> IO (Maybe Exception)
withdraw _cache conn iban amount = do
  ma <- DB.accountByIban conn iban
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

          txId <- DB.insertTXLine conn newTxLine
          print txId

          DB.updateAccountBalance conn aid newBalance

          return Nothing

transfer :: AppCache 
         -> SqlBackend
         -> T.Text 
         -> T.Text 
         -> Double 
         -> T.Text 
         -> IO (Maybe Exception)
transfer _cache conn fromIban toIban amount reference = do
  mFrom <- DB.accountByIban conn fromIban
  case mFrom of 
    Nothing -> return $ Just AccountNotFound
    (Just (Entity fromAid fromAccount)) -> do
      mTo <- DB.accountByIban conn toIban
      case mTo of 
        Nothing -> return $ Just AccountNotFound
        (Just (Entity toAid toAccount)) -> do
          now <- getCurrentTime

          -- TODO: fetch user for name in TXLine
          let nameStr = "Transfer"

          -- TODO: if accounts belong to different customers, then check if giro and below 5000
          
          let newFromTxLine  = TXLine fromAid toIban (-amount) nameStr reference now
              newToTxLine    = TXLine toAid fromIban amount nameStr reference now

          _ <- DB.insertTXLine conn newFromTxLine
          _ <- DB.insertTXLine conn newToTxLine
          
          DB.updateAccountBalance conn fromAid (accountBalance fromAccount - amount)
          DB.updateAccountBalance conn toAid (accountBalance toAccount - amount)

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
