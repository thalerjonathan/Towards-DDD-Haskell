{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
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

import Database.PostgreSQL.Simple
import Database.Persist.Postgresql
import Data.Pool(Pool, withResource)
import Conduit
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as C
import Infrastructure.DB.Config
import Data.String
import Control.Monad.Reader

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
      putStrLn "Before Set schema"
      ret <- execute_ conn setSchemaQuery
      putStrLn $ "After Set schema, returned " ++ show ret
  
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