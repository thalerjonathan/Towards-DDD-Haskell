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
import Application.DTO

import Database.Persist.Postgresql
import Infrastructure.Cache.AppCache 
import Application.Banking
import Infrastructure.DB.Pool as Pool

-- TODO https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html

-- TODO: use template haskell to annotate functions with REST endpoints just like in Spring
-- and generate the REST API and all code for handling it automatically
-- https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial#:~:text=Template%20Haskell%20(TH)%20is%20the,the%20results%20of%20their%20execution.
-- TODO: put Servant API definition directly here

handleAllCustomers :: AppCache
                   -> DbPool 
                   -> Handler [CustomerDetailsDTO]
handleAllCustomers cache p = 
  liftIO $ Pool.runWithTX p (getAllCustomers cache)

handleCustomer :: AppCache
               -> DbPool
               -> T.Text
               -> Handler CustomerDTO
handleCustomer cache p customerId = do
  ret <- liftIO $ Pool.runWithTX p (getCustomer cache customerId)
  case ret of 
    (Left _) -> throwError err404
    (Right cust) -> return cust

handleAccount :: AppCache
              -> DbPool
              -> T.Text
              -> Handler AccountDTO
handleAccount cache p iban = do
  ret <- liftIO $ Pool.runWithTX p (getAccount cache iban)
  case ret of 
    (Left _) -> throwError err404
    (Right a) -> return a

handleDeposit :: AppCache
              -> DbPool
              -> T.Text 
              -> Double 
              -> Handler (Either T.Text TXLineDTO)
handleDeposit cache p iban amount = 
  performAccountTx p (deposit cache iban amount) 

handleWithdraw :: AppCache
               -> DbPool
               -> T.Text 
               -> Double 
               -> Handler (Either T.Text TXLineDTO)
handleWithdraw cache p iban amount = 
  performAccountTx p (withdraw cache iban amount) 

handleTransfer :: AppCache
               -> DbPool
               -> T.Text 
               -> T.Text 
               -> Double 
               -> T.Text 
               -> Handler (Either T.Text TXLineDTO)
handleTransfer cache p fromIban toIban amount reference = 
  performAccountTx p (transferEventual cache fromIban toIban amount reference) 

handleSwagger :: Handler Swagger
handleSwagger = return bankingSwagger


performAccountTx :: DbPool
                 -> (SqlBackend -> IO (Either Exception TXLineDTO)) 
                 -> Handler (Either T.Text TXLineDTO)
performAccountTx p act = do
  ret <- liftIO $ Pool.runWithTX p act
  case ret of 
    (Left (InvalidAccountOperation str)) -> return $ Left str
    (Left _)   -> throwError err404
    (Right tx) -> return $ Right tx