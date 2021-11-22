{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Infrastructure.DB.MySQLPool 
  ( MySqlPool
  , getMySqlPool
  , initSQLPool
  ) where

import Database.Persist.MySQL
import Data.Pool(Pool)
import Conduit
import Control.Monad.Logger
import qualified Database.MySQL.Base as MySQL

import Infrastructure.DB.DbConfig

newtype MySqlPool = P (Pool SqlBackend)

poolConnections :: Int
poolConnections = 5

getMySqlPool :: MySqlPool -> Pool SqlBackend
getMySqlPool (P p) = p

initSQLPool :: (MonadUnliftIO m, MonadLogger m) => DbConfig -> m MySqlPool
initSQLPool cfg = do
    -- https://hackage.haskell.org/package/mysql-0.1.4/docs/Database-MySQL-Base.html
    -- See http://www.yesodweb.com/blog/2016/11/use-mysql-safely-in-yesod
    liftIO MySQL.initLibrary
    P <$> createMySQLPool mySqlConn poolConnections
  where
    mySqlConn = 
        defaultConnectInfo {
          connectHost     = dbHost cfg
        , connectPort     = fromIntegral $ dbPort cfg
        , connectUser     = dbUser cfg
        , connectPassword = dbPassword cfg
        , connectDatabase = dbSchema cfg
        }