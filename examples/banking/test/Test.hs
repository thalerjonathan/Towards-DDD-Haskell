module Main where

import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import BDD.Parsing.Gherkin
import BDD.Data.Gherkin
import BDD.Data.Step

import Control.Monad.Logger
import Control.Monad.Except
import Data.Either.Combinators

import View.Rest.Api
import Application.Banking
import Infrastructure.Cache.AppCache
import Infrastructure.DB.PgPool
import qualified Infrastructure.DB.DbConfig as DbCfg

import System.IO.Unsafe

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
    (Right f) -> do
      eCfgs <- runExceptT loadConfigs
      case eCfgs of
        (Left err) -> putStrLn $ "Failed loading configs: \n  " ++ err ++ "\n\nExit!"
        (Right dbBankingCfg) -> do
          dbPool <- runStdoutLoggingT $ initPool dbBankingCfg
          cache  <- mkAppCache

          let _stepsScenario1 =
                      [ (Given 
                          (Text "my Giro account has a balance of" 
                            (Param Double StepEnd)), 
                          givenGiroAccountBalance cache dbPool 1234.56)
                      , (When 
                          (Text "I deposit" 
                            (Param Double 
                              (Text "into my account" StepEnd))), 
                          whenDepositBalance cache dbPool 567.89)
                      , (Then 
                          (Text "I should have a balance of" 
                            (Param Double 
                              (Text "in my account" StepEnd))), 
                          thenExpectNewBalance cache dbPool 1802.45)
                      ]

          let _stepsScenario2 =
                      [ (Given 
                          (Text "a Savings account with Iban" 
                            (Param Word 
                              (Text "and a balance of" 
                                (Param Double StepEnd)))), 
                          givenGiroAccountBalance cache dbPool 1234.56)

                      , (Given 
                          (Text "a Giro account with Iban" 
                            (Param Word 
                              (Text "and a balance of" 
                                (Param Double StepEnd)))), 
                          givenGiroAccountBalance cache dbPool 1234.56)

                      , (When 
                          (Text "Transferring" 
                            (Param Double 
                              (Text "from Iban" 
                                (Param Word
                                  (Text "to Iban"
                                    (Param Word StepEnd)))))), 
                          givenGiroAccountBalance cache dbPool 1234.56)

                      , (Then 
                          (Text "I expect the error" 
                            (Param Word StepEnd)), 
                          givenGiroAccountBalance cache dbPool 1234.56)

                      , (Then 
                          (Text "There should be a balance of" 
                            (Param Double 
                              (Text "in the account with Iban"
                                (Param Word StepEnd)))), 
                          givenGiroAccountBalance cache dbPool 1234.56)

                      , (Then 
                          (Text "There should be a balance of" 
                            (Param Double 
                              (Text "in the account with Iban"
                                (Param Word StepEnd)))), 
                          givenGiroAccountBalance cache dbPool 1234.56)
                      ]

          let beforeScenario = putStrLn "Before Scenario"
          let afterScenario = putStrLn "After Scenario"
          runFeature f beforeScenario afterScenario _stepsScenario1

runFeature :: Feature 
           -> IO ()
           -> IO ()
           -> [(StepType, IO ())] 
           -> IO ()
runFeature f beforeScenario afterScenario steps = do
  -- TODO: run each scenario in separate transaction and roll back at the end to avoid persistent changes to DB. Implement through BeforeScenario

  let (Feature _ _ ss) = f

  mapM_ (\s -> do 
    beforeScenario
    runScenario steps s
    afterScenario) ss

  return ()

runScenario :: [(StepType, IO ())] -> Scenario -> IO ()
runScenario steps (Scenario _ (G givenStep (W whenStep (T thenStep)))) = do
    putStrLn $ "Given: " ++ show givenStep
    putStrLn $ "When: " ++ show whenStep
    putStrLn $ "Then: " ++ show thenStep
    
    putStrLn ""

    mapM_ (\(t, _) -> print t) steps

    putStrLn ""

    givenRet <- execStepActionsFor steps givenStep "Given"
    if not givenRet 
      then return ()
      else do
        whenRet <- execStepActionsFor steps whenStep "When"
        if not whenRet
          then return ()
          else do
            _thenRet <- execStepActionsFor steps thenStep "Then"
            return ()

    return ()

data StepActionParam
  = ParamWord String
  | ParamInt Int
  | ParamDouble Double
  deriving Show

execStepActionsFor :: [(StepType, IO ())] -> Step -> String -> IO Bool
execStepActionsFor steps (Step stepStr stepAnd) st = do
    let stepDefs = map (\(s, act) -> (stepDefinition s, act)) $ filter (\(s, _) -> isStepType st s) steps

    case findFirstParse stepDefs stepStr of
      Nothing  -> do
        print $ "Error: could not find matching action for " ++ st ++ ": " ++ stepStr
        return False
      (Just (params, _action)) -> do
        print $ "Found matching " ++ st ++ " action for " ++ show stepStr ++ ", parsed params: " ++ show params
        -- TODO: execute action
        execStepActionsForAnd stepDefs stepAnd
     
    -- mapM_ (\def -> parseTest (parseStep def []) str) stepDefs
    
  where
    execStepActionsForAnd :: [(StepDefinition, IO ())] -> And -> IO Bool
    execStepActionsForAnd _ NoAnd = return True
    execStepActionsForAnd stepDefs (And andStr andAnd) = do
      -- mapM_ (\def -> parseTest (parseStep def []) andStr) stepDefs
      case findFirstParse stepDefs andStr of
        Nothing -> do
          print $ "Error: could not find matching action for " ++ st ++ " And:" ++ andStr
          return False
        (Just (params, _action)) -> do
          print $ "Found matching " ++ st ++ " And action for " ++ show andStr ++ ", parsed params: " ++ show params
          -- TODO: execute action
          execStepActionsForAnd stepDefs andAnd

    findFirstParse :: [(StepDefinition, IO ())] -> String -> Maybe ([StepActionParam], IO ())
    findFirstParse (sd:sds) parseStr = 
        case parse (parseStep sd []) "" parseStr of 
          (Left _err)  -> unsafePerformIO (do
            --putStrLn ""
            --print _err
            return $ findFirstParse sds parseStr)
          (Right p) -> Just p
    findFirstParse _ _ = Nothing 

    isStepType :: String -> StepType -> Bool
    isStepType "Given" (Given _) = True
    isStepType "When" (When _)   = True
    isStepType "Then" (Then _)   = True
    isStepType _ _               = False

    stepDefinition :: StepType -> StepDefinition
    stepDefinition (Given def) = def
    stepDefinition (When def)  = def
    stepDefinition (Then def)  = def

    parseStep :: (StepDefinition, IO ()) -> [StepActionParam] -> Parser ([StepActionParam], IO ()) 
    parseStep ((Text s cont), act) acc = do
      hspace
      _ <- string s
      hspace
      parseStep (cont, act) acc

    parseStep ((Param Word cont), act) acc = do
      _ <- char '\''
      w <- some (alphaNumChar <|> spaceChar <|> char '!')
      _ <- char '\''
      parseStep (cont, act) (acc ++ [ParamWord w])

    parseStep ((Param Int cont), act) acc = do
      i <- some digitChar
      parseStep (cont, act) (acc ++ [ParamInt $ read i])

    parseStep ((Param Double cont), act) acc = do
      p <- some digitChar
      _ <- char '.'
      c <- some digitChar
      parseStep (cont, act) (acc ++ [ParamDouble $ read (p ++ ['.'] ++ c)])

    parseStep (StepEnd, act) acc = return (acc, act)

type Parser = Parsec Void String


-- Given my Giro account has a balance of {double}
-- Given - "my Giro account has a balance of" - Double
givenGiroAccountBalance :: AppCache
                        -> PgPool
                        -> Double 
                        -> IO ()
givenGiroAccountBalance cache pool balance = do
  let iban = "AT99 99999 9999999999"
  owner <- createCustomer cache pool "Jonathan"
  _ <- createAccount cache pool owner iban balance "GIRO"
  return ()

-- When I deposit {double} into my account
whenDepositBalance :: AppCache
                   -> PgPool
                   -> Double 
                   -> IO ()
whenDepositBalance cache pool balance = do
  let iban = "AT99 99999 9999999999"
  _ <- deposit cache pool iban balance
  return ()

-- Then I should have a balance of {double} in my account
thenExpectNewBalance :: AppCache
                     -> PgPool
                     -> Double 
                     -> IO ()
thenExpectNewBalance cache pool expectedBalance = do
  let iban = "AT99 99999 9999999999"
  ret <- getAccount cache pool iban
  case ret of
    (Left _) -> Prelude.error "Account Not Found!"
    (Right a) -> do
      let balance = accountDetailBalance (accountDetails a)
      if (abs (balance - expectedBalance) > 0.01)
        then Prelude.error $ "Expected balance " ++ show expectedBalance ++ " but was " ++ show balance
        else return ()

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
