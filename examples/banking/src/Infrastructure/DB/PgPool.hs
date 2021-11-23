{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Infrastructure.DB.PgPool
  ( PgPool
  , getPool
  , initPool
  ) where

import Database.PostgreSQL.Simple
import Database.Persist.Postgresql
import Data.Pool(Pool)
import Conduit
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as C
import Infrastructure.DB.DbConfig
import Data.String

newtype PgPool = P (Pool SqlBackend)

poolConnections :: Int
poolConnections = 5

getPool :: PgPool -> Pool SqlBackend
getPool (P p) = p

initPool :: (MonadUnliftIO m, MonadLogger m) => DbConfig -> m PgPool
initPool cfg = P <$> createPostgresqlPoolModified connAct connStr poolConnections
  where
    setSchemaQuery :: Query
    setSchemaQuery = fromString $ "SET SCHEMA '" ++ (dbSchema cfg) ++ "';"

    connAct conn = do
      ret <- execute_ conn setSchemaQuery
      print ret
      return ()
  
    connStr = C.pack $ 
      "host="      ++ dbHost cfg ++ 
      " port="     ++ show (dbPort cfg) ++
      " user="     ++ dbUser cfg ++ 
      " dbname="   ++ dbName cfg ++
      " password=" ++ dbPassword cfg