module Main where

import Domain.Account
import Data.UUID.V4 (nextRandom)
import Domain.Customer (CustomerId(CustomerId))

import Control.Monad.Logger
import Control.Monad.Except
import Data.Either.Combinators
import Infrastructure.Cache.AppCache

import qualified Infrastructure.Web.Banking as Banking
import qualified Infrastructure.Web.Server as Server
import qualified Infrastructure.DB.MySQLPool as MySQLPool
import qualified Infrastructure.DB.DbConfig as DbCfg

import Data.Time
import Data.Fixed

dbBankingCfgFile :: String
dbBankingCfgFile = "db.banking.conf"

type AppConfig = DbCfg.DbConfig

main :: IO ()
main = do
  eCfgs <- runExceptT loadConfigs
  case eCfgs of
    (Left err) -> putStrLn $ "Failed loading configs: \n  " ++ err ++ "\n\nExit!"
    (Right dbBankingCfg) -> do
      dbPool <- runStdoutLoggingT $ MySQLPool.initSQLPool dbBankingCfg
      cache  <- mkAppCache
      
      uuid <- nextRandom
  
      let cid = CustomerId uuid
          i   = Iban "AT12 12345 01234567890"
          a0  = account cid i

      -- (a1, (ret, es)) <- execCommand a0 (Deposit 100)
      -- putStrLn $ "Account emitted DomainEvents: " ++ show es
      -- putStrLn $ "Account returned: " ++ show ret

      -- (_a2, (ret', es')) <- execCommand a1 (Withdraw 2000)
      -- putStrLn $ "Account emitted DomainEvents: " ++ show es'
      -- putStrLn $ "Account returned: " ++ show ret'

      let cmds = 
            [ Deposit 100
            , Withdraw 2000
            , TransferTo i 1000 "Jonathan" "Rent" 
            , ReceiveFrom i 1000 "Jonathan" "Rent" ]

      (rets, es) <- execCommands a0 cmds
      putStrLn $ "Account emitted DomainEvents: " ++ show es
      putStrLn $ "Account returned: " ++ show rets

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