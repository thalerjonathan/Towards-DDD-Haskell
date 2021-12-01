module Application.Async
  ( eventProcessor
  ) where

import Data.Text.Encoding as T
import Control.Concurrent
import Control.Monad
import Data.Aeson

import Application.Banking
import Application.DomainEvents
import Infrastructure.DB.Pool as Pool
import Infrastructure.DB.Banking as DB

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
      case persistedEventType pe of
        "TransferSent" -> do
          let mevt = decodeStrict (T.encodeUtf8 $ persistedEventPayload pe) :: Maybe TransferSentEvent
          case mevt of 
            Nothing -> return ()
            (Just evt) -> do
              processDomainEvent (TransferSent evt) conn
              DB.markEventProcessed pid conn
        "TransferFailed" -> do
          let mevt = decodeStrict (T.encodeUtf8 $ persistedEventPayload pe) :: Maybe TransferFailedEvent
          case mevt of 
            Nothing -> return ()
            (Just evt) -> do
              processDomainEvent (TransferFailed evt) conn
              DB.markEventProcessed pid conn
        _ -> do
          DB.markEventFailed pid "Unknown Event" conn