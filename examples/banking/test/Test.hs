module Main where

import Text.Megaparsec hiding (State)

import BDD.Parsing.Gherkin
import BDD.Steps.AccountSteps
import BDD.Runner

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

main :: IO ()
main = do
  let _scenario1 = "Feature: Depositing money into accounts\n" ++
                   "In order to manage my money more efficiently\n" ++
                   "As a bank client\n" ++
                   "I want to deposit money into my accounts whenever I need to.\n" ++
                   "Scenario: Deposit money into a Giro account\n" ++
                   "Given my Giro account has a balance of 1234.56\n" ++
                   "When I deposit 567.89 into my account\n" ++ 
                   "Then I should have a balance of 1802.45 in my account\n"

  let _scenario2 = "Feature: Transfering money between accounts\n" ++
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

  --s <- readFile "test/resources/features/Depositing.feature"
  --print s
  let ret = parse parseGherkin "" _scenario1
  case ret of
    (Left e) -> print e
    (Right feat) -> do
      eCfgs <- runExceptT loadConfigs
      case eCfgs of
        (Left err) -> putStrLn $ "Failed loading configs: \n  " ++ err ++ "\n\nExit!"
        (Right dbBankingCfg) -> do
          dbPool <- runStdoutLoggingT $ initPool dbBankingCfg
          cache  <- mkAppCache

          let _stepsScenario1 =
                      [ (givenGiroAccountBalanceStep, 
                         givenGiroAccountBalance cache)
                      , 
                        (whenDepositBalanceStep,
                         whenDepositBalance cache)
                      , 
                        ( thenExpectNewBalanceStep,
                          thenExpectNewBalance cache)
                      ]

          -- let _stepsScenario2 =
          --             [ (Given 
          --                 (Text "a Savings account with Iban" 
          --                   (Param Word 
          --                     (Text "and a balance of" 
          --                       (Param Double StepEnd)))), 
          --                 givenGiroAccountBalance cache dbPool 1234.56)

          --             , (Given 
          --                 (Text "a Giro account with Iban" 
          --                   (Param Word 
          --                     (Text "and a balance of" 
          --                       (Param Double StepEnd)))), 
          --                 givenGiroAccountBalance cache dbPool 1234.56)

          --             , (When 
          --                 (Text "Transferring" 
          --                   (Param Double 
          --                     (Text "from Iban" 
          --                       (Param Word
          --                         (Text "to Iban"
          --                           (Param Word StepEnd)))))), 
          --                 givenGiroAccountBalance cache dbPool 1234.56)

          --             , (Then 
          --                 (Text "I expect the error" 
          --                   (Param Word StepEnd)), 
          --                 givenGiroAccountBalance cache dbPool 1234.56)

          --             , (Then 
          --                 (Text "There should be a balance of" 
          --                   (Param Double 
          --                     (Text "in the account with Iban"
          --                       (Param Word StepEnd)))), 
          --                 givenGiroAccountBalance cache dbPool 1234.56)

          --             , (Then 
          --                 (Text "There should be a balance of" 
          --                   (Param Double 
          --                     (Text "in the account with Iban"
          --                       (Param Word StepEnd)))), 
          --                 givenGiroAccountBalance cache dbPool 1234.56)
          --             ]

          runFeature 
            feat
            (\scenarioAction -> do
              putStrLn "Before Scenario"
              
              runWithConnection dbPool $ \conn -> do 
                putStrLn "BEGIN TX"
                beginTX conn
                _ <- scenarioAction conn
                putStrLn "ROLLBACK TX"
                liftIO $ runReaderT transactionUndo conn

              putStrLn "After Scenario"
            ) _stepsScenario1

dbBankingCfgFile :: String
dbBankingCfgFile = "db.banking.conf"

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
