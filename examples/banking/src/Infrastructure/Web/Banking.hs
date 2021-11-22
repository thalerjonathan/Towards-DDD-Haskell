{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
module Infrastructure.Web.Banking 
  ( banking
  ) where

import Servant

import qualified View.Rest.Api as Api
import qualified View.Rest.Handlers as Handlers

import Infrastructure.Cache.AppCache
import Infrastructure.DB.MySQLPool

banking :: AppCache
        -> MySqlPool
        -> Application
banking cache dbPool
    = serve Api.bankingApi server
  where
    server :: Server Api.BankingApi
    server 
        = Handlers.handleSwagger
          :<|> Handlers.handleAllCustomers cache dbPool
          :<|> Handlers.handleCustomer cache dbPool 
          :<|> Handlers.handleAccount cache dbPool
          :<|> Handlers.handleDeposit cache dbPool
          :<|> Handlers.handleWithdraw cache dbPool
          :<|> Handlers.handleTransfer cache dbPool