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
import Data.Text
import Control.Monad.IO.Class

import qualified View.Rest.Api as Api

import Infrastructure.Cache.AppCache 
import Infrastructure.DB.MySQLPool

-- TODO https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html

-- TODO: use template haskell to annotate functions with REST endpoints just like in Spring
-- and generate the REST API and all code for handling it automatically
-- https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial#:~:text=Template%20Haskell%20(TH)%20is%20the,the%20results%20of%20their%20execution.
-- TODO: put Servant API definition directly here
handleAllCustomers :: AppCache
                   -> MySqlPool 
                   -> Handler [Api.CustomerDetailsDTO]
handleAllCustomers _cache _dbPool = do
  liftIO $ print ("handleAllCustomers" :: String)
  return []

handleCustomer :: AppCache
               -> MySqlPool
               -> Text
               -> Handler Api.CustomerDTO
handleCustomer _cache _dbPool _customerId = undefined

handleAccount :: AppCache
              -> MySqlPool
              -> Text
              -> Handler Api.AccountDTO
handleAccount _cache _dbPool _accountIban = undefined

handleDeposit :: AppCache
              -> MySqlPool
              -> Text 
              -> Double 
              -> Handler Api.CommandResponse
handleDeposit _cache _dbPool _accountIban _amount  = undefined

handleWithdraw :: AppCache
               -> MySqlPool
               -> Text 
               -> Double 
               -> Handler Api.CommandResponse
handleWithdraw _cache _dbPool _iban _amount  = undefined

handleTransfer :: AppCache
               -> MySqlPool
               -> Text 
               -> Text 
               -> Double 
               -> Text 
               -> Handler Api.CommandResponse
handleTransfer _cache _dbPool _fromIban _toIban _amount _reference = undefined

handleSwagger :: Handler Swagger
handleSwagger = return Api.bankingSwagger