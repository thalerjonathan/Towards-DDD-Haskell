module View.HTML.Controller where

import Servant (Handler)
import Text.Blaze.Html
import Control.Monad.IO.Class
import Data.Text as T

import qualified Infrastructure.DB.Pool as Pool
import Application.Banking
import Infrastructure.Cache.AppCache (AppCache)

import View.HTML.Templates.AllCustomers
import View.HTML.Templates.Customer

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
  liftIO $ print $ ("handleCustomer " <> customerId)
  ret <- liftIO $ Pool.runWithTX p (getCustomer cache customerId)
  case ret of 
    (Left _)  -> do
      liftIO $ putStrLn "fuck"
      undefined -- TODO: redirect to error page
    (Right c) -> return (customerHtml c)