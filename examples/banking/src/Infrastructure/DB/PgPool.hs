{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Infrastructure.DB.PgPool
  ( PgPool
  , getPool
  , initPool
  , runWithConnection
  ) where

import Database.PostgreSQL.Simple
import Database.Persist.Postgresql
import Data.Pool(Pool, withResource)
import Conduit
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as C
import Infrastructure.DB.DbConfig
import Data.String

newtype PgPool = P (Pool SqlBackend)

poolConnections :: Int
poolConnections = 5

runWithConnection :: PgPool -> (SqlBackend -> IO a) -> IO a
runWithConnection (P p) act = withResource p act

getPool :: PgPool -> Pool SqlBackend
getPool (P p) = p

initPool :: (MonadUnliftIO m, MonadLoggerIO m) => DbConfig -> m PgPool
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