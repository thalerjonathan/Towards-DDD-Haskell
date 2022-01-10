{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
module Application.Eff.Layer where

import           Application.Events
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Text                     as T
import           Data.Time.Clock
import           Data.UUID
import           Data.UUID.V4
import           Database.Persist.Postgresql
import           Infrastructure.Cache.AppCache
import           Infrastructure.DB.Banking     as DB
import qualified Infrastructure.DB.Pool        as Pool

data AppData = AppData
  { _conn  :: SqlBackend
  , _cache :: AppCache
  }

newtype AppCtx a = AppCtx (ReaderT AppData (WriterT [IO ()] IO) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader AppData
        , MonadWriter [IO ()]
        , MonadIO
        )

data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving Show

class Monad m => ApplicationLayer m where
  persistDomainEvent :: DomainEvent -> m ()
  logging            :: LogLevel -> T.Text -> m ()
  nextUUID           :: m UUID

instance ApplicationLayer AppCtx where
  persistDomainEvent :: DomainEvent -> AppCtx ()
  persistDomainEvent evt = do
    conn <- asks _conn
    now <- liftIO getCurrentTime
    let (evtName, payload) = toPayload evt
    let pe = PersistedEventEntity now evtName False False "" payload
    _ <- liftIO $ DB.insertEvent pe conn
    return ()

  logging :: LogLevel -> T.Text -> AppCtx ()
  logging lvl txt = liftIO $ putStrLn $ "LOG " ++ show lvl ++ ": " ++ show txt

  nextUUID :: AppCtx UUID
  nextUUID = liftIO nextRandom

runApplicationTX :: Pool.DbPool -> AppCache -> AppCtx a -> IO a
runApplicationTX p cache (AppCtx prog) = do
  (ret, postTxActions) <- Pool.runWithTX p (\conn -> do
    let appData = AppData conn cache
    runWriterT $ runReaderT prog appData)

  sequence_ postTxActions
  return ret
