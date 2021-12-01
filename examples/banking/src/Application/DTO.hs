{-# LANGUAGE DeriveGeneric  #-}
module Application.DTO where

import Data.Typeable (Typeable)
import GHC.Generics

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
  , accountDetailBalance :: Double
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
  , txLineAmount    :: Double
  , txLineTime      :: UTCTime
  } deriving (Eq, Show, Generic, Typeable)
