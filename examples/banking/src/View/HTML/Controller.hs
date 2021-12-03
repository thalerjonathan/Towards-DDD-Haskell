module View.HTML.Controller where

import           Control.Monad.IO.Class
import           Data.Text                     as T
import           Servant                       (Handler)
import           Text.Blaze.Html

import           Application.Banking
import qualified Application.BankingNew        as New
import           Domain.Application            as App
import           Infrastructure.Cache.AppCache (AppCache)
import qualified Infrastructure.DB.Pool        as Pool

import           View.HTML.Forms
import           View.HTML.Page.Account
import           View.HTML.Page.AllCustomers
import           View.HTML.Page.Customer
import           View.HTML.Page.Error
import           View.HTML.Page.Redirect

handleAllCustomers :: AppCache
                   -> Pool.DbPool
                   -> Handler Html
handleAllCustomers _cache p = do
  --cs <- liftIO $ Pool.runWithTX p (getAllCustomers cache)
  cs <- liftIO $ Pool.runWithTX p (App.runApplication New.getAllCustomers)
  return (allCustomersHtml cs)

handleCustomer :: AppCache
               -> Pool.DbPool
               -> T.Text
               -> Handler Html
handleCustomer _cache p customerId = do
  -- ret <- liftIO $ Pool.runWithTX p (getCustomer cache customerId)
  ret <- liftIO $ Pool.runWithTX p (App.runApplication (New.getCustomer customerId))
  case ret of
    (Left err) ->
      redirectToError $ exceptionToErrorMessage err
    (Right c)  ->
      return (customerHtml c)

handleAccount :: AppCache
              -> Pool.DbPool
              -> T.Text
              -> T.Text
              -> T.Text
              -> Handler Html
handleAccount _cache p accIban customerId customerName = do
  -- ret <- liftIO $ Pool.runWithTX p (getAccount cache accIban)
  ret <- liftIO $ Pool.runWithTX p (App.runApplication (New.getAccount accIban))
  case ret of
    (Left err) ->
      redirectToError $ exceptionToErrorMessage err
    (Right a)  ->
      return (accountHtml customerId customerName a)

handleDeposit :: AppCache
              -> Pool.DbPool
              -> AccountForm
              -> Handler Html
handleDeposit cache p form = do
  let iban   = accountFormIban form
      amount = accountFormAmount form

  ret <- liftIO $ Pool.runWithTX p (deposit cache iban amount)
  case ret of
    (Left err) ->
      redirectToError $ exceptionToErrorMessage err
    (Right _) ->
      redirectToAccount form

handleWithdraw :: AppCache
               -> Pool.DbPool
               -> AccountForm
               -> Handler Html
handleWithdraw cache p form = do
  let iban   = accountFormIban form
      amount = accountFormAmount form
  ret <- liftIO $ Pool.runWithTX p (withdraw cache iban amount)
  case ret of
    (Left err) ->
      redirectToError $ exceptionToErrorMessage err
    (Right _) ->
      redirectToAccount form

handleTransfer :: AppCache
               -> Pool.DbPool
               -> TransferForm
               -> Handler Html
handleTransfer cache p form = do
    let fromIban  = transferFormFromIban form
        toIban    = transferFormToIban form
        amount    = transferFormAmount form
        reference = transferFormReference form
    ret <- liftIO $ Pool.runWithTX p (transferEventual cache fromIban toIban amount reference)
    case ret of
      (Left err) ->
        redirectToError $ exceptionToErrorMessage err
      (Right _) ->
        redirectToAccount (transferToAccountForm form)
  where
    transferToAccountForm :: TransferForm -> AccountForm
    transferToAccountForm t = AccountForm
      { accountFormCustomerId   = transferFormCustomerId t
      , accountFormCustomerName = transferFormCustomerName t
      , accountFormIban         = transferFormFromIban t
      , accountFormAmount       = 0.0
      }

handleError :: Text -> Handler Html
handleError msg = return $ errorPageHtml msg

exceptionToErrorMessage :: Exception -> Text
exceptionToErrorMessage CustomerNotFound              = "Customer not found!"
exceptionToErrorMessage AccountNotFound               = "Account not found!"
exceptionToErrorMessage (InvalidAccountOperation err) = err

redirectToError :: Text -> Handler Html
redirectToError msg = return $ redirectToHtml $ "/error?msg=" <> msg

redirectToAccount :: AccountForm -> Handler Html
redirectToAccount form = return $ redirectToHtml (accountPageLink (accountFormIban form) (accountFormCustomerId form) (accountFormCustomerName form))

accountPageLink :: Text -> Text -> Text -> Text
accountPageLink accountIban customerId customerName
  = "/account?iban=" <> accountIban <>
    "&id=" <> customerId <>
    "&name=" <> customerName
