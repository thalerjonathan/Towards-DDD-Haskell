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
import Application.Banking
import Infrastructure.DB.PgPool

-- TODO https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html

-- TODO: use template haskell to annotate functions with REST endpoints just like in Spring
-- and generate the REST API and all code for handling it automatically
-- https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial#:~:text=Template%20Haskell%20(TH)%20is%20the,the%20results%20of%20their%20execution.
-- TODO: put Servant API definition directly here
handleAllCustomers :: AppCache
                   -> PgPool 
                   -> Handler [CustomerDetailsDTO]
handleAllCustomers c p = liftIO $ getAllCustomers c p

handleCustomer :: AppCache
               -> PgPool
               -> T.Text
               -> Handler CustomerDTO
handleCustomer c p customerId = do
  ret <- liftIO $ getCustomer c p customerId
  case ret of 
    (Left _) -> throwError err404
    (Right cust) -> return cust

handleAccount :: AppCache
              -> PgPool
              -> T.Text
              -> Handler AccountDTO
handleAccount c p iban = do
  ret <- liftIO $ getAccount c p iban
  case ret of 
    (Left _) -> throwError err404
    (Right a) -> return a

handleDeposit :: AppCache
              -> PgPool
              -> T.Text 
              -> Double 
              -> Handler CommandResponse
handleDeposit c p iban amount = do
  ret <- liftIO $ deposit c p iban amount
  case ret of 
    (Just (InvalidAccountOperation str)) -> return $ CommandResponse False (Just str)
    (Just _) -> throwError err404
    Nothing -> return $ CommandResponse True Nothing

handleWithdraw :: AppCache
               -> PgPool
               -> T.Text 
               -> Double 
               -> Handler CommandResponse
handleWithdraw c p iban amount = do
  ret <- liftIO $ withdraw c p iban amount
  case ret of 
    (Just (InvalidAccountOperation str)) -> return $ CommandResponse False (Just str)
    (Just _) -> throwError err404
    Nothing -> return $ CommandResponse True Nothing

handleTransfer :: AppCache
               -> PgPool
               -> T.Text 
               -> T.Text 
               -> Double 
               -> T.Text 
               -> Handler CommandResponse
handleTransfer cache dbPool fromIban toIban amount reference = do
  ret <- liftIO $ transfer cache dbPool fromIban toIban amount reference
  case ret of 
    (Just (InvalidAccountOperation str)) -> return $ CommandResponse False (Just str)
    (Just _) -> throwError err404
    Nothing -> return $ CommandResponse True Nothing

handleSwagger :: Handler Swagger
handleSwagger = return bankingSwagger