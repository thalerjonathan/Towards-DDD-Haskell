{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module View.Rest.Api where

import Control.Lens
import Data.Aeson
import Servant
import Servant.Swagger
import Data.Swagger
import Data.Typeable (Typeable)
import GHC.Generics

import Data.Text

import Application.DTO

data CommandResponse = CommandResponse
  { ok    :: Bool
  , error :: Maybe Text
  } deriving (Eq, Show, Generic, Typeable)

instance ToJSON CustomerDetailsDTO
instance ToJSON CustomerDTO
instance ToJSON AccountDetailsDTO
instance ToJSON AccountDTO
instance ToJSON TXLineDTO
instance ToJSON CommandResponse

instance FromJSON CustomerDetailsDTO
instance FromJSON CustomerDTO
instance FromJSON AccountDetailsDTO
instance FromJSON AccountDTO
instance FromJSON TXLineDTO
instance FromJSON CommandResponse

instance ToSchema CustomerDetailsDTO where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
instance ToSchema CustomerDTO where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
instance ToSchema AccountDetailsDTO where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
instance ToSchema AccountDTO where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
instance ToSchema TXLineDTO where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
instance ToSchema CommandResponse where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy

type GetAllCustomersRest = "rest" :> "customer" :> "all" :> Get '[JSON] [CustomerDetailsDTO]
type GetCustomerRest     = "rest" :> "customer" :> Capture ":id" Text :> Get '[JSON] CustomerDTO
type GetAccountRest      = "rest" :> "account" :> Capture ":iban" Text :> Get '[JSON] AccountDTO
type PostDepositRest     = "rest" :> "account" :> Capture ":iban" Text :> "deposit" :> Capture ":amount" Double :> Post '[JSON] CommandResponse
type PostWithdrawRest    = "rest" :> "account" :> Capture ":iban" Text :> "withdraw" :> Capture ":amount" Double :> Post '[JSON] CommandResponse
type PostTransferRest    = "rest" :> "account" :> Capture ":fromIban" Text :> "transfer" :> Capture ":toIban" Text :> Capture ":amount" Double :> Capture ":reference" Text :> Post '[JSON] CommandResponse

type BankingRestApi 
  = GetAllCustomersRest
  :<|> GetCustomerRest
  :<|> GetAccountRest
  :<|> PostDepositRest
  :<|> PostWithdrawRest
  :<|> PostTransferRest

type SwaggerApi = "api" :> "v1" :> "swagger.json" :> Get '[JSON] Swagger

bankingRestApi :: Proxy BankingRestApi
bankingRestApi = Proxy

bankingSwagger :: Swagger
bankingSwagger = toSwagger bankingRestApi
  & info.title   .~ "Banking API"
  & info.version .~ "1.0"
  & info.description ?~ "This is the REST API for Banking"
  & info.license ?~ ("BSD 3.0" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")