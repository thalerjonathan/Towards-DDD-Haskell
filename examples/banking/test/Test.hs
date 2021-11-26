module Main where

import Text.Megaparsec hiding (State)

import Test.Cucumber.Data.Step
import Test.Cucumber.Parsing.Gherkin
import Test.Cucumber.Runner
import Test.Cucumber

import Steps.AccountSteps

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Except
import Data.Either.Combinators

import Database.Persist.Sql
import Infrastructure.Cache.AppCache
import Infrastructure.DB.PgPool
import qualified Infrastructure.DB.DbConfig as DbCfg
import Infrastructure.DB.BankingDb as DB

type AppConfig = DbCfg.DbConfig

testFeature1 :: String
testFeature1 = "Feature: Depositing money into accounts\n" ++
                  "In order to manage my money more efficiently\n" ++
                  "As a bank client\n" ++
                  "I want to deposit money into my accounts whenever I need to.\n" ++
                "Scenario: Deposit money into a Giro account\n" ++
                  "Given my Giro account has a balance of 1234.56\n" ++
                  "When I deposit 567.89 into my account\n" ++ 
                  "Then I should have a balance of 1802.45 in my account\n"

testFeature2 :: String
testFeature2 = "Feature: Transfering money between accounts\n" ++
                   "In order to manage my money more efficiently\n" ++
                   "As a bank client\n" ++
                   "I want to transfer money between accounts whenever I need to.\n" ++
              "Scenario: Transfer money from a Savings account to another Giro account of different customers\n" ++ 
                "Given a Savings account with Iban 'AT12 12345 01234567890' and a balance of 1000.0\n" ++
                "And a Giro account with Iban 'AT98 98765 09876543210' and a balance of 500.0\n" ++
                "When Transferring 200.0 from Iban 'AT12 12345 01234567890' to Iban 'AT98 98765 09876543210'\n" ++
                "Then I expect the error 'Cannot transfer from Savings account!'\n" ++
                "And There should be a balance of 1000.0 in the account with Iban 'AT12 12345 01234567890'\n" ++
                "And There should be a balance of 500.0 in the account with Iban 'AT98 98765 09876543210'\n"

feature1Steps :: [(StepType, StepAction AccountStepsData)]
feature1Steps = [ (givenGiroAccountBalanceStep, 
                      givenGiroAccountBalance)
                  , 
                    (whenDepositBalanceStep,
                      whenDepositBalance)
                  , 
                    ( thenExpectNewBalanceStep,
                      thenExpectNewBalance)
                  ]

main :: IO ()
main = do
  --s <- readFile "test/resources/features/Depositing.feature"
  --print s
  let ret = parse parseGherkin "" testFeature1
  case ret of
    (Left e) -> print e
    (Right feature) -> do
      eCfgs <- runExceptT loadConfigs
      case eCfgs of
        (Left err) -> putStrLn $ "Failed loading configs: \n  " ++ err ++ "\n\nExit!"
        (Right dbBankingCfg) -> do
          dbPool <- runStdoutLoggingT $ initPool dbBankingCfg
          cache  <- mkAppCache

          runFeature 
            feature
            (\scenarioAction -> do
              putStrLn "Before Scenario"
              
              runWithConnection dbPool $ \conn -> do 
                putStrLn "BEGIN TX"
                beginTX conn

                let s = AccountStepsData {
                    accountStepDataIban  = "AT99 99999 9999999999"
                  , accountStepDataCache = cache
                  , accountStepDataConn  = conn
                  }

                void $ scenarioAction s
                putStrLn "ROLLBACK TX"
                liftIO $ runReaderT transactionUndo conn

              putStrLn "After Scenario"
            ) feature1Steps

dbBankingCfgFile :: String
dbBankingCfgFile = "db.banking.conf"

loadConfigs :: ExceptT String IO AppConfig
loadConfigs = toExceptT (dbBankingCfgFile ++ ": ") $ DbCfg.loadDBCfg dbBankingCfgFile
  where
    toExceptT :: String -> IO (Either String a) -> ExceptT String IO a
    toExceptT str act = do
      ret <- liftIO act
      let ret' = mapLeft (str ++) ret
      ExceptT (return ret')
