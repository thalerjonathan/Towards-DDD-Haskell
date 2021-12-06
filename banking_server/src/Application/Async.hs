module Application.Async
  ( eventProcessor
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Data.Text.Encoding        as T

import           Application.BankingAnemic
import           Application.DomainEvents
import           Infrastructure.DB.Banking as DB
import           Infrastructure.DB.Pool    as Pool

eventProcessor :: Pool.DbPool -> Int -> IO ()
eventProcessor p interval = void $ forkIO $ forever $ do
  threadDelay interval
  processNextEvent p

processNextEvent :: Pool.DbPool -> IO ()
processNextEvent p = runWithTX p $ \conn -> do
  mpe <- DB.nextEvent conn
  case mpe of
    Nothing -> return ()
    (Just (Entity pid pe)) -> do
      case persistedEventEntityType pe of
        "TransferSent" -> do
          let mevt = decodeStrict (T.encodeUtf8 $ persistedEventEntityPayload pe) :: Maybe TransferSentEventData
          case mevt of
            Nothing -> return ()
            (Just evt) -> do
              processDomainEvent (TransferSent evt) conn
              DB.markEventProcessed pid conn
        "TransferFailed" -> do
          let mevt = decodeStrict (T.encodeUtf8 $ persistedEventEntityPayload pe) :: Maybe TransferFailedEventData
          case mevt of
            Nothing -> return ()
            (Just evt) -> do
              processDomainEvent (TransferFailed evt) conn
              DB.markEventProcessed pid conn
        _ -> do
          DB.markEventFailed pid "Unknown Event" conn
