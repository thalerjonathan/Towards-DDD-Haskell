{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
module View.HTML.Api where

import Data.Text
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html (Html)
import GHC.Generics
import Web.FormUrlEncoded

data AccountForm = AccountForm 
  { accountFormCustomerId   :: Text
  , accountFormCustomerName :: Text
  , accountFormIban         :: Text
  , accountFormAmount       :: Double
  } deriving (Eq, Show, Generic)

data TransferForm = TransferForm 
  { transferFormCustomerId   :: Text
  , transferFormCustomerName :: Text
  , transferFormFromIban     :: Text
  , transferFormToIban       :: Text
  , transferFormAmount       :: Double
  , transferFormReference    :: Text
  } deriving (Eq, Show, Generic)

instance FromForm AccountForm where
  fromForm f = AccountForm
    <$> parseUnique "customerId" f
    <*> parseUnique "customerName" f
    <*> parseUnique "iban" f
    <*> parseUnique "amount" f

instance FromForm TransferForm where
  fromForm f = TransferForm
    <$> parseUnique "customerId" f
    <*> parseUnique "customerName" f
    <*> parseUnique "fromIban" f
    <*> parseUnique "receivingIban" f
    <*> parseUnique "amount" f
    <*> parseUnique "reference" f

type GetAllCustomersHtml = Get '[HTML] Html

type GetCustomerHtml = "customer" :> QueryParam' '[Required, Strict] "id" Text :> Get '[HTML] Html

type GetAccountHtml = "account" :> QueryParam' '[Required, Strict] "iban" Text 
                                :> QueryParam' '[Required, Strict] "id" Text 
                                :> QueryParam' '[Required, Strict] "name" Text :> Get '[HTML] Html

type PostDepositHtml = "account" :> "deposit" :> ReqBody '[FormUrlEncoded] AccountForm :> Post '[HTML] Html
type PostWithdrawHtml = "account" :> "withdraw" :> ReqBody '[FormUrlEncoded] AccountForm :> Post '[HTML] Html
type PostTransferHtml = "account" :> "transfer" :> ReqBody '[FormUrlEncoded] TransferForm :> Post '[HTML] Html