module Test.BDD.Tests
  ( bdd_tests
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Logger
import           Data.Either.Combinators
import           Database.Persist.Sql
import           Infrastructure.Cache.AppCache
import qualified Infrastructure.DB.Config       as DbCfg
import           Infrastructure.DB.Pool
import           Test.BDD.Steps.DepositWithdraw as DW
import           Test.BDD.Steps.Transferring    as TR
import           Test.Cucumber
import           Test.Tasty
import           Test.Tasty.HUnit

type AppConfig = DbCfg.DbConfig

bdd_tests :: TestTree
bdd_tests = testGroup "BDD Tests"
              [ testDeposit
              , testWithdraw
              , testTransferring ]

testDeposit :: TestTree
testDeposit =
  testScenario
    [ DW.givenAccount
    , DW.whenDeposit
    , DW.thenExpectBalance
    , DW.thenExpectError
    ]
    "Deposit Scenarios"
    "test/resources/features/Depositing.feature"
    (\cache conn -> DW.StepData {
        DW.stepDataIban  = "AT99 99999 9999999999"
      , DW.stepDataCache = cache
      , DW.stepDataConn  = conn
      , DW.stepDataEx    = Nothing
      })

testWithdraw :: TestTree
testWithdraw =
  testScenario
    [ DW.givenAccount
    , DW.whenWithdraw
    , DW.thenExpectBalance
    , DW.thenExpectError
    ]
    "Withdraw Scenarios"
    "test/resources/features/Withdrawing.feature"
    (\cache conn -> DW.StepData {
        DW.stepDataIban  = "AT99 99999 9999999999"
      , DW.stepDataCache = cache
      , DW.stepDataConn  = conn
      , DW.stepDataEx    = Nothing
      })

testTransferring :: TestTree
testTransferring =
  testScenario
    [ TR.givenAccount
    , TR.givenAccountSameCustomer
    , TR.whenTransferring
    , TR.thenExpectBalance
    , TR.thenExpectError
    ]
    "Transferring Scenarios"
    "test/resources/features/Transferring.feature"
    (\cache conn -> TR.StepData {
        TR.stepDataOwner = Nothing
      , TR.stepDataCache = cache
      , TR.stepDataConn  = conn
      , TR.stepDataEx    = Nothing
      })

testScenario :: [(StepType, StepAction s)]
             -> String
             -> String
             -> (AppCache -> SqlBackend -> s)
             -> TestTree
testScenario steps testCaseName file f = testCase testCaseName $ do
    feat <- readFile file
    let ret = parseFeature feat
    case ret of
      (Left err) -> error $ show err
      (Right feature) -> do
        eCfgs <- runExceptT loadConfigs
        case eCfgs of
          (Left err) -> error err
          (Right dbBankingCfg) -> do
            dbPool <- runStdoutLoggingT $ initPool dbBankingCfg
            cache  <- mkAppCache

            runFeature
              feature
              (\scenarioAction -> do
                runTXWithRollback dbPool $ \conn -> do
                  let s = f cache conn
                  void $ scenarioAction s
              ) steps
  where
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