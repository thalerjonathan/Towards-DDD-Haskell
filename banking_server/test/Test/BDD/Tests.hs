module Test.BDD.Tests
  ( bdd_tests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Test.Cucumber

import           Test.BDD.Steps.Deposit

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Logger
import           Data.Either.Combinators

import           Infrastructure.Cache.AppCache
import qualified Infrastructure.DB.Config      as DbCfg
import           Infrastructure.DB.Pool

type AppConfig = DbCfg.DbConfig

depositSteps :: [(StepType, StepAction DepositStepData)]
depositSteps = [ givenAccount
               , whenDeposit
               , thenDeposit
               , thenExpectError
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
  depositFeature <- readFile "test/resources/features/Depositing.feature"
  let ret = parseFeature depositFeature
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
                  , depositException     = Nothing
                  }

                void $ scenarioAction s

              putStrLn "After Scenario"
            ) depositSteps

bdd_tests :: TestTree
bdd_tests = testGroup "BDD Tests"
              [ depositScenarios ]
