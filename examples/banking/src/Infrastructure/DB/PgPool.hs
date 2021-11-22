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

import Database.Persist.Postgresql
import Data.Pool(Pool)
import Conduit
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as C

import Infrastructure.DB.DbConfig

newtype PgPool = P (Pool SqlBackend)

poolConnections :: Int
poolConnections = 5

getPool :: PgPool -> Pool SqlBackend
getPool (P p) = p

initPool :: (MonadUnliftIO m, MonadLogger m) => DbConfig -> m PgPool
initPool cfg = P <$> createPostgresqlPool connStr poolConnections
  where
    connStr = C.pack $ 
      "host="      ++ dbHost cfg ++ 
      " port="     ++ show (dbPort cfg) ++
      " user="     ++ dbUser cfg ++ 
      " dbname="   ++ dbSchema cfg ++
      " password=" ++ dbPassword cfg