module Infrastructure.DB.DbConfig
  ( DbConfig ( dbUser, dbPassword, dbSchema, dbHost, dbPort)
  , loadDBCfg
  ) where

import qualified Data.Configurator as Cfg
import Data.Either.Combinators

data DbConfig = DbConfig 
  { dbUser     :: String
  , dbPassword :: String
  , dbSchema   :: String
  , dbHost     :: String
  , dbPort     :: Int
  } deriving (Show)

loadDBCfg :: String -> IO (Either String DbConfig)
loadDBCfg file = do
  conf     <- Cfg.load [Cfg.Required file]

  user   <- Cfg.lookup conf "db.user"   :: IO (Maybe String)
  pw     <- Cfg.lookup conf "db.pw"     :: IO (Maybe String)
  schema <- Cfg.lookup conf "db.schema" :: IO (Maybe String)
  host   <- Cfg.lookup conf "db.host"   :: IO (Maybe String)
  port   <- Cfg.lookup conf "db.port"   :: IO (Maybe Int)
  
  return $ DbConfig <$> maybeToRight "db.user is missing" user
                    <*> maybeToRight "db.pw is missing" pw
                    <*> maybeToRight "db.schema is missing" schema
                    <*> maybeToRight "db.host is missing" host
                    <*> maybeToRight "db.port is missing" port