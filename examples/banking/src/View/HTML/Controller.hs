module View.HTML.Controller where

import Servant (Handler)
import Text.Blaze.Html
import Control.Monad.IO.Class
import Data.Text as T

import qualified Infrastructure.DB.Pool as Pool
import Application.Banking
import Infrastructure.Cache.AppCache (AppCache)

import View.HTML.Api
import View.HTML.Templates.AllCustomers
import View.HTML.Templates.Customer
import View.HTML.Templates.Account

handleAllCustomers :: AppCache
                   -> Pool.DbPool 
                   -> Handler Html
handleAllCustomers cache p = do
  cs <- liftIO $ Pool.runWithTX p (getAllCustomers cache)
  return (allCustomersHtml cs)

handleCustomer :: AppCache
               -> Pool.DbPool
               -> T.Text
               -> Handler Html
handleCustomer cache p customerId = do
  ret <- liftIO $ Pool.runWithTX p (getCustomer cache customerId)
  case ret of 
    (Left _)  -> do
      liftIO $ putStrLn "fuck"
      undefined -- TODO: redirect to error page
    (Right c) -> return (customerHtml c)

handleAccount :: AppCache
              -> Pool.DbPool
              -> T.Text
              -> T.Text
              -> T.Text
              -> Handler Html
handleAccount cache p accIban customerId customerName = do
  ret <- liftIO $ Pool.runWithTX p (getAccount cache accIban)
  case ret of 
    (Left _)  -> do
      liftIO $ putStrLn "fuck"
      undefined -- TODO: redirect to error page
    (Right a) -> return (accountHtml customerId customerName a) 

handleDeposit :: AppCache
              -> Pool.DbPool
              -> AccountForm
              -> Handler Html
handleDeposit cache p form = do
  let iban   = accountFormIban form
      amount = accountFormAmount form
  ret <- liftIO $ Pool.runWithTX p (deposit cache iban amount)
  case ret of 
    (Just (InvalidAccountOperation _err)) -> do
      liftIO $ putStrLn "fuck"
      undefined -- TODO: redirect to error page
    (Just _err) -> do
      liftIO $ putStrLn "fuck"
      undefined -- TODO: redirect to error page
    Nothing -> do
      -- TODO: redirect to account
      undefined

handleWithdraw :: AppCache
               -> Pool.DbPool
               -> AccountForm
               -> Handler Html
handleWithdraw cache p form = do
  let iban   = accountFormIban form
      amount = accountFormAmount form
  ret <- liftIO $ Pool.runWithTX p (withdraw cache iban amount)
  case ret of 
    (Just (InvalidAccountOperation _err)) -> do
      liftIO $ putStrLn "fuck"
      undefined -- TODO: redirect to error page
    (Just _err) -> do
      liftIO $ putStrLn "fuck"
      undefined -- TODO: redirect to error page
    Nothing -> do
      -- TODO: redirect to account
      undefined

handleTransfer :: AppCache
               -> Pool.DbPool
               -> TransferForm
               -> Handler Html
handleTransfer cache p form = do
  let fromIban  = transferFormFromIban form
      toIban    = transferFormToIban form
      amount    = transferFormAmount form
      reference = transferFormReference form
  ret <- liftIO $ Pool.runWithTX p (transfer cache fromIban toIban amount reference)
  case ret of 
    (Just (InvalidAccountOperation _err)) -> do
      liftIO $ putStrLn "fuck"
      undefined -- TODO: redirect to error page
    (Just _err) -> do
      liftIO $ putStrLn "fuck"
      undefined -- TODO: redirect to error page
    Nothing -> do
      -- TODO: redirect to account
      undefined