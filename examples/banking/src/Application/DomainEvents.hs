{-# LANGUAGE DeriveGeneric #-}
module Application.DomainEvents where

import GHC.Generics
import Data.Aeson

import Data.Text as T

data DomainEvent
  = TransferSent TransferSentEvent
  | TransferFailed TransferFailedEvent

data TransferSentEvent = TransferSentEvent 
  { transferSentEventAmount            :: Double
  , transferSentEventReference         :: T.Text
  , transferSentEventSendingCustomer   :: T.Text
  , transferSentEventReceivingCustomer :: T.Text
  , transferSentEventSendingAccount    :: T.Text
  , transferSentEventReceivingAccount  :: T.Text
  } deriving (Eq, Show, Generic)

data TransferFailedEvent = TransferFailedEvent 
  { transferFailedEventError             :: T.Text
  , transferFailedEventAmount            :: Double
  , transferFailedEventReference         :: T.Text
  , transferFailedEventSendingCustomer   :: T.Text
  , transferFailedEventReceivingCustomer :: T.Text
  , transferFailedEventSendingAccount    :: T.Text
  , transferFailedEventReceivingAccount  :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON TransferSentEvent
instance ToJSON TransferFailedEvent

instance FromJSON TransferSentEvent
instance FromJSON TransferFailedEvent