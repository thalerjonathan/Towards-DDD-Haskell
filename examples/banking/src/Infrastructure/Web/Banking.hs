{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
module Infrastructure.Web.Banking 
  ( banking
  ) where

import Servant

import qualified View.Api as Api
import qualified View.Rest.Handlers as Rest
import qualified View.HTML.Controller as Html

import Infrastructure.Cache.AppCache
import Infrastructure.DB.Pool

banking :: AppCache
        -> DbPool
        -> Application
banking cache dbPool
    = serve Api.bankingApi server
  where
    server :: Server Api.BankingApi
    server 
        = Rest.handleSwagger
          :<|> Rest.handleAllCustomers cache dbPool
          :<|> Rest.handleCustomer cache dbPool 
          :<|> Rest.handleAccount cache dbPool
          :<|> Rest.handleDeposit cache dbPool
          :<|> Rest.handleWithdraw cache dbPool
          :<|> Rest.handleTransfer cache dbPool
          :<|> Html.handleAllCustomers cache dbPool
          :<|> Html.handleCustomer cache dbPool
          :<|> Html.handleAccount cache dbPool
          :<|> Html.handleDeposit cache dbPool
          :<|> Html.handleWithdraw cache dbPool
          :<|> Html.handleTransfer cache dbPool
          :<|> Html.handleError