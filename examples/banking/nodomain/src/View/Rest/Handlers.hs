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
              -> Handler CommandResponse
handleDeposit cache p iban amount = do
  ret <- liftIO $ Pool.runWithTX p (deposit cache iban amount)
  case ret of 
    (Just (InvalidAccountOperation str)) -> return $ CommandResponse False (Just str)
    (Just _) -> throwError err404
    Nothing -> return $ CommandResponse True Nothing

handleWithdraw :: AppCache
               -> DbPool
               -> T.Text 
               -> Double 
               -> Handler CommandResponse
handleWithdraw cache p iban amount = do
  ret <- liftIO $ Pool.runWithTX p (withdraw cache iban amount)
  case ret of 
    (Just (InvalidAccountOperation str)) -> return $ CommandResponse False (Just str)
    (Just _) -> throwError err404
    Nothing -> return $ CommandResponse True Nothing

handleTransfer :: AppCache
               -> DbPool
               -> T.Text 
               -> T.Text 
               -> Double 
               -> T.Text 
               -> Handler CommandResponse
handleTransfer cache p fromIban toIban amount reference = do
  ret <- liftIO $ Pool.runWithTX p $ (transfer cache fromIban toIban amount reference)
  case ret of 
    (Just (InvalidAccountOperation str)) -> return $ CommandResponse False (Just str)
    (Just _) -> throwError err404
    Nothing -> return $ CommandResponse True Nothing

handleSwagger :: Handler Swagger
handleSwagger = return bankingSwagger