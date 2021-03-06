{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Infrastructure.DB.Pool
  ( DbPool
  , getPool
  , initPool
  , runWithConnection
  , runNoTX
  , runWithTX
  , runTXWithRollback

  , beginTX
  , rollbackTX
  , commitTX
  ) where

import           Conduit
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.ByteString.Char8       as C
import           Data.Pool                   (Pool, withResource)
import           Data.String
import           Database.Persist.Postgresql
import           Database.PostgreSQL.Simple
import           Infrastructure.DB.Config

newtype DbPool = P (Pool SqlBackend)

poolConnections :: Int
poolConnections = 5

getPool :: DbPool -> Pool SqlBackend
getPool (P p) = p

initPool :: (MonadUnliftIO m, MonadLoggerIO m) => DbConfig -> m DbPool
initPool cfg = P <$> createPostgresqlPoolModified connAct connStr poolConnections
  where
    -- NOTE: need to enclose in BEGIN / COMMIT to avoid 'WARNING: there is no transaction in progress'
    setSchemaQuery :: Query
    setSchemaQuery = fromString $ "BEGIN; SET SCHEMA '" ++ (dbSchema cfg) ++ "'; COMMIT;"

    connAct conn = do
      --putStrLn "Before Set schema"
      _ret <- execute_ conn setSchemaQuery
      --putStrLn $ "After Set schema, returned " ++ show ret
      return ()

    connStr = C.pack $
      "host="      ++ dbHost cfg ++
      " port="     ++ show (dbPort cfg) ++
      " user="     ++ dbUser cfg ++
      " dbname="   ++ dbName cfg ++
      " password=" ++ dbPassword cfg

runWithConnection :: DbPool -> (SqlBackend -> IO a) -> IO a
runWithConnection (P p) act = withResource p act

runNoTX :: DbPool -> (SqlBackend -> IO a) -> IO a
runNoTX p act = runSqlPoolNoTransaction (do
  conn <- ask
  liftIO $ act conn) (getPool p) Nothing -- (Just ReadUncommitted)

runWithTX :: DbPool -> (SqlBackend -> IO a) -> IO a
runWithTX p act = runSqlPool (do
  conn <- ask
  liftIO $ act conn) (getPool p)

runTXWithRollback :: DbPool -> (SqlBackend -> IO a) -> IO a
runTXWithRollback p act = runWithConnection p (\conn -> do
  beginTX conn
  ret <- act conn
  rollbackTX conn
  return ret)

beginTX :: SqlBackend -> IO ()
beginTX = liftIO . runReaderT (rawExecute "BEGIN" [])

rollbackTX :: SqlBackend -> IO ()
rollbackTX = liftIO . runReaderT (rawExecute "ROLLBACK" [])

commitTX :: SqlBackend -> IO ()
commitTX = liftIO . runReaderT (rawExecute "COMMIT" [])
