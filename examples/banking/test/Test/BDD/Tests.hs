module Test.BDD.Tests
  ( bdd_tests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Text.Megaparsec               hiding (State)

import           Test.Cucumber
import           Test.Cucumber.Data.Step
import           Test.Cucumber.Parsing.Gherkin
import           Test.Cucumber.Runner

import           Test.BDD.Steps.Deposit

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Logger
import           Data.Either.Combinators

import           Infrastructure.Cache.AppCache
import qualified Infrastructure.DB.Config      as DbCfg
import           Infrastructure.DB.Pool

type AppConfig = DbCfg.DbConfig

depositFeature :: String
depositFeature = "Feature: Depositing money into accounts\n" ++
                  "In order to manage my money more efficiently\n" ++
                  "As a bank client\n" ++
                  "I want to deposit money into my accounts whenever I need to.\n" ++
                "Scenario: Deposit money into a Giro account\n" ++
                  "Given my Giro account has a balance of 1234.56\n" ++
                  "When I deposit 567.89 into my account\n" ++
                  "Then I should have a balance of 1802.45 in my account\n"

depositSteps :: [(StepType, StepAction DepositStepData)]
depositSteps = [ (givenDepositStep, givenDeposit)
               , (whenDepositStep, whenDeposit)
               , (thenDepositStep, thenDeposit)
               ]

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

depositScenarios :: TestTree
depositScenarios = testCase "Deposit Scenarios" $ do
  --s <- readFile "test/resources/features/Depositing.feature"
  --print s
  let ret = parse parseGherkin "" depositFeature
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

              runTXWithRollback dbPool $ \conn -> do
                let s = DepositStepData {
                    depositStepDataIban  = "AT99 99999 9999999999"
                  , depositStepDataCache = cache
                  , depositStepDataConn  = conn
                  }

                void $ scenarioAction s

              putStrLn "After Scenario"
            ) depositSteps

bdd_tests :: TestTree
bdd_tests = testGroup "BDD Tests"
              [ depositScenarios ]
