module View.Rest.Handlers 
  ( handleAllCustomers
  , handleCustomer
  , handleAccount
  , handleDeposit
  , handleWithdraw
  , handleTransfer
  , handleSwagger
  ) where

import Servant
import Data.Swagger
import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Time.Clock

import View.Rest.Api

import Infrastructure.Cache.AppCache 
import Infrastructure.DB.PgPool
import Infrastructure.DB.BankingDb as DB

-- TODO https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html

-- TODO: use template haskell to annotate functions with REST endpoints just like in Spring
-- and generate the REST API and all code for handling it automatically
-- https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial#:~:text=Template%20Haskell%20(TH)%20is%20the,the%20results%20of%20their%20execution.
-- TODO: put Servant API definition directly here
handleAllCustomers :: AppCache
                   -> PgPool 
                   -> Handler [CustomerDetailsDTO]
handleAllCustomers _cache dbPool = do
  cs <- liftIO $ DB.allCustomers dbPool
  return $ map customerEntityToDetailsDTO cs

handleCustomer :: AppCache
               -> PgPool
               -> T.Text
               -> Handler CustomerDTO
handleCustomer _cache dbPool domainId = do
  mc <- liftIO $ DB.customerById dbPool domainId
  case mc of 
    Nothing -> throwError err404
    (Just c@(Entity cid _)) -> do
      as <- liftIO $ DB.accountsOfCustomer dbPool cid 
      return $ customerEntityToDTO c as

handleAccount :: AppCache
              -> PgPool
              -> T.Text
              -> Handler AccountDTO
handleAccount _cache dbPool iban = do
  ma <- liftIO $ DB.accountByIban dbPool iban
  case ma of 
    Nothing -> throwError err404
    (Just a@(Entity aid _)) -> do
      txs <- liftIO $ DB.txLinesOfAccount dbPool aid
      return $ accountEntityToDTO a txs

handleDeposit :: AppCache
              -> PgPool
              -> T.Text 
              -> Double 
              -> Handler CommandResponse
handleDeposit _cache dbPool iban amount = do
  ma <- liftIO $ DB.accountByIban dbPool iban
  case ma of 
    Nothing -> throwError err404
    (Just (Entity aid a)) -> do
      -- TODO: check Account Type! only allowed for Giro!

      now <- liftIO getCurrentTime
      let newBalance = amount + accountBalance a 
          newTxLine  = TXLine aid iban amount "Deposit" "Deposit" now

      txId <- liftIO $ DB.insertTXLine dbPool newTxLine
      liftIO $ print txId
      liftIO $ DB.updateAccountBalance dbPool aid newBalance
      return $ CommandResponse True Nothing

handleWithdraw :: AppCache
               -> PgPool
               -> T.Text 
               -> Double 
               -> Handler CommandResponse
handleWithdraw _cache dbPool iban amount = do
  ma <- liftIO $ DB.accountByIban dbPool iban
  case ma of 
    Nothing -> throwError err404
    (Just (Entity aid a)) -> do
      -- TODO: check Account Type! only allowed for Giro!
      let newBalance = accountBalance a - amount
      if newBalance < -1000
        then return $ CommandResponse False (Just "Cannot overdraw Giro account by more than -1000.0!")
        else do
          now <- liftIO getCurrentTime
          let newTxLine  = TXLine aid iban (-amount) "Deposit" "Deposit" now

          txId <- liftIO $ DB.insertTXLine dbPool newTxLine
          liftIO $ print txId

          liftIO $ DB.updateAccountBalance dbPool aid newBalance
          return $ CommandResponse True Nothing

handleTransfer :: AppCache
               -> PgPool
               -> T.Text 
               -> T.Text 
               -> Double 
               -> T.Text 
               -> Handler CommandResponse
handleTransfer _cache dbPool fromIban toIban amount reference = do
  mFrom <- liftIO $ DB.accountByIban dbPool fromIban
  case mFrom of 
    Nothing -> throwError err404
    (Just (Entity fromAid fromAccount)) -> do
      mTo <- liftIO $ DB.accountByIban dbPool toIban
      case mTo of 
        Nothing -> throwError err404
        (Just (Entity toAid toAccount)) -> do
          now <- liftIO getCurrentTime

          -- TODO: fetch user for name in TXLine
          let nameStr = "Transfer"

          -- TODO: if accounts belong to different customers, then check if giro and below 5000
          
          let newFromTxLine  = TXLine fromAid toIban (-amount) nameStr reference now
              newToTxLine    = TXLine toAid fromIban amount nameStr reference now

          _ <- liftIO $ DB.insertTXLine dbPool newFromTxLine
          _ <- liftIO $ DB.insertTXLine dbPool newToTxLine
          
          liftIO $ DB.updateAccountBalance dbPool fromAid (accountBalance fromAccount - amount)
          liftIO $ DB.updateAccountBalance dbPool toAid (accountBalance toAccount - amount)

          return $ CommandResponse True Nothing

handleSwagger :: Handler Swagger
handleSwagger = return bankingSwagger

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
