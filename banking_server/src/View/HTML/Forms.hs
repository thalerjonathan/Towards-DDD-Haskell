{-# LANGUAGE DeriveGeneric      #-}
module View.HTML.Forms where

import Data.Text

import GHC.Generics

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