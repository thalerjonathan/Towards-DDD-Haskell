{-# LANGUAGE DeriveGeneric #-}
module Application.Events where

import           Data.Aeson
import           Data.Aeson.Text as Aeson
import           GHC.Generics

import           Data.Text       as T
import qualified Data.Text.Lazy  as TL

data DomainEvent
  = TransferSent TransferSentEventData
  | TransferFailed TransferFailedEventData

data TransferSentEventData = TransferSentEventData
  { transferSentEventAmount            :: Double
  , transferSentEventReference         :: T.Text
  , transferSentEventSendingCustomer   :: T.Text
  , transferSentEventReceivingCustomer :: T.Text
  , transferSentEventSendingAccount    :: T.Text
  , transferSentEventReceivingAccount  :: T.Text
  } deriving (Eq, Show, Generic)

data TransferFailedEventData = TransferFailedEventData
  { transferFailedEventError             :: T.Text
  , transferFailedEventAmount            :: Double
  , transferFailedEventReference         :: T.Text
  , transferFailedEventSendingCustomer   :: T.Text
  , transferFailedEventReceivingCustomer :: T.Text
  , transferFailedEventSendingAccount    :: T.Text
  , transferFailedEventReceivingAccount  :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON TransferSentEventData
instance ToJSON TransferFailedEventData

instance FromJSON TransferSentEventData
instance FromJSON TransferFailedEventData

toPayload :: DomainEvent -> (T.Text, T.Text)
toPayload (TransferSent d)   = ("TransferSent", TL.toStrict $ Aeson.encodeToLazyText d)
toPayload (TransferFailed d) = ("TransferFailed", TL.toStrict $ Aeson.encodeToLazyText d)
