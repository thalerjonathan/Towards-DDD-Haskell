{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
module View.Rest.Api where

import Control.Lens
import Data.Aeson
import GHC.Generics
import Data.Typeable (Typeable)
import Servant
import Servant.Swagger
import Data.Swagger

import Data.Text
import Data.Time.Clock

data CustomerDetailsDTO = CustomerDetailsDTO
  { customerDetailsId   :: Text
  , customerDetailsName :: Text
  } deriving (Eq, Show, Generic, Typeable)

data CustomerDTO = CustomerDTO
  { customerDetails        :: CustomerDetailsDTO
  , customerAccountDetails :: [AccountDetailsDTO]
  } deriving (Eq, Show, Generic, Typeable)

data AccountDetailsDTO = AccountDetailsDTO 
  { accountDetailIban    :: Text
  , accountDetailBalance :: Text
  , accountDetailType    :: Text
  } deriving (Eq, Show, Generic, Typeable)

data AccountDTO = AccountDTO 
  { accountDetails :: AccountDetailsDTO
  , accountTXLines :: [TXLineDTO]
  } deriving (Eq, Show, Generic, Typeable)

data TXLineDTO = TXLineDTO 
  { txLineIban      :: Text
  , txLineName      :: Text
  , txLineReference :: Text
  , txLineAmount    :: Text
  , txLineTime      :: UTCTime
  } deriving (Eq, Show, Generic, Typeable)

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

type GetAllCustomers  = "rest" :> "customer" :> "all" :> Get '[JSON] [CustomerDetailsDTO]
type GetCustomer      = "rest" :> "customer" :> Capture ":id" Text :> Get '[JSON] CustomerDTO
type GetAccount       = "rest" :> "account" :> Capture ":iban" Text :> Get '[JSON] AccountDTO
type PutDeposit       = "rest" :> "account" :> Capture ":iban" Text :> "deposit" :> Capture ":amount" Double :> Put '[JSON] CommandResponse
type PutWithdraw      = "rest" :> "account" :> Capture ":iban" Text :> "withdraw" :> Capture ":amount" Double :> Put '[JSON] CommandResponse
type PutTransfer      = "rest" :> "account" :> Capture ":fromIban" Text :> "transfer" :> Capture ":toIban" Text :> Capture ":amount" Double :> Capture ":reference" Text :> Put '[JSON] CommandResponse

type BankingRestApi 
  = GetAllCustomers 
  :<|> GetCustomer
  :<|> GetAccount
  :<|> PutDeposit
  :<|> PutWithdraw
  :<|> PutTransfer

type SwaggerApi = "api" :> "v1" :> "swagger.json" :> Get '[JSON] Swagger

type BankingApi  = (SwaggerApi 
                  :<|> GetAllCustomers 
                  :<|> GetCustomer
                  :<|> GetAccount
                  :<|> PutDeposit
                  :<|> PutWithdraw
                  :<|> PutTransfer)

bankingRestApi :: Proxy BankingRestApi
bankingRestApi = Proxy

bankingApi :: Proxy BankingApi
bankingApi = Proxy

bankingSwagger :: Swagger
bankingSwagger = toSwagger bankingRestApi
  & info.title   .~ "Banking API"
  & info.version .~ "1.0"
  & info.description ?~ "This is the REST API for Banking"
  & info.license ?~ ("BSD 3.0" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")