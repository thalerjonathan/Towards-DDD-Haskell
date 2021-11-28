{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
module View.Api where

import View.Rest.Api
import View.HTML.Api

import Servant

type BankingApi = (SwaggerApi 
                  :<|> GetAllCustomersRest 
                  :<|> GetCustomerRest
                  :<|> GetAccountRest
                  :<|> PostDepositRest
                  :<|> PostWithdrawRest
                  :<|> PostTransferRest
                  :<|> GetAllCustomersHtml
                  :<|> GetCustomerHtml
                  :<|> GetAccountHtml
                  :<|> PostDepositHtml
                  :<|> PostWithdrawHtml
                  :<|> PostTransferHtml
                  :<|> GetError)

bankingApi :: Proxy BankingApi
bankingApi = Proxy