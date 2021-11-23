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

customerEntityToDetailsDTO :: Entity Customer -> CustomerDetailsDTO
customerEntityToDetailsDTO (Entity _ c) = CustomerDetailsDTO 
  { customerDetailsId = customerDomainId c
  , customerDetailsName = customerName c
  }

customerEntityToDTO :: Entity Customer -> [Entity Account] -> CustomerDTO
customerEntityToDTO c as = CustomerDTO 
  { customerDetails = customerEntityToDetailsDTO c
  , customerAccountDetails = map accountEntityToDetailsDTO as
  }

accountEntityToDetailsDTO :: Entity Account -> AccountDetailsDTO
accountEntityToDetailsDTO (Entity _ a) = AccountDetailsDTO 
  { accountDetailIban = accountIban a
  , accountDetailBalance = accountBalance a
  , accountDetailType = accountType a
  }

handleAccount :: AppCache
              -> PgPool
              -> T.Text
              -> Handler AccountDTO
handleAccount _cache _dbPool _accountIban = undefined

handleDeposit :: AppCache
              -> PgPool
              -> T.Text 
              -> Double 
              -> Handler CommandResponse
handleDeposit _cache _dbPool _accountIban _amount  = undefined

handleWithdraw :: AppCache
               -> PgPool
               -> T.Text 
               -> Double 
               -> Handler CommandResponse
handleWithdraw _cache _dbPool _iban _amount  = undefined

handleTransfer :: AppCache
               -> PgPool
               -> T.Text 
               -> T.Text 
               -> Double 
               -> T.Text 
               -> Handler CommandResponse
handleTransfer _cache _dbPool _fromIban _toIban _amount _reference = undefined

handleSwagger :: Handler Swagger
handleSwagger = return bankingSwagger