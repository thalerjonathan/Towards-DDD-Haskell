module Main where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Data.Either.Combinators
import           Infrastructure.Cache.AppCache

-- import qualified Application.Async             as Async
import qualified Infrastructure.DB.Config      as DbCfg
import qualified Infrastructure.DB.Pool        as Pool
import qualified Infrastructure.Web.Banking    as Banking
import qualified Infrastructure.Web.Server     as Server

import           Data.Fixed
import           Data.Time

type AppConfig = DbCfg.DbConfig

dbBankingCfgFile :: String
dbBankingCfgFile = "db.banking.conf"

eventPollInterval :: Int
eventPollInterval = 10000000

main :: IO ()
main = do
  eCfgs <- runExceptT loadConfigs
  case eCfgs of
    (Left err) -> putStrLn $ "Failed loading configs: \n  " ++ err ++ "\n\nExit!"
    (Right dbBankingCfg) -> do
      dbPool <- runStdoutLoggingT $ Pool.initPool dbBankingCfg
      cache  <- mkAppCache

      -- Async.eventProcessor dbPool eventPollInterval

      Server.startServer (Banking.banking cache dbPool)

-- TODO: https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/
-- TODO: https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/
-- TODO: https://www.fpcomplete.com/haskell/tutorial/exceptions/
-- TODO: https://markkarpov.com/tutorial/exceptions.html
-- TODO: https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html
loadConfigs :: ExceptT String IO AppConfig
loadConfigs = do
    dbBankingCfg <- toExceptT (dbBankingCfgFile ++ ": ") $ DbCfg.loadDBCfg dbBankingCfgFile
    return dbBankingCfg
  where
    toExceptT :: String -> IO (Either String a) -> ExceptT String IO a
    toExceptT str act = do
      ret <- liftIO act
      let ret' = mapLeft (str ++) ret
      ExceptT (return ret')

-- https://wiki.haskell.org/Time
-- https://williamyaoh.com/posts/2019-09-16-time-cheatsheet.html
mkUTCTime :: Integer
          -> Int
          -> Int
          -> Int
          -> Int
          -> Pico
          -> UTCTime
mkUTCTime year mon day hour mn sec =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour mn sec))