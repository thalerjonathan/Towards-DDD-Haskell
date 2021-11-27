module Steps.DepositSteps where

import Data.Text
import Test.Cucumber.Data.Step

import Control.Monad
import Control.Monad.State (gets)

import Database.Persist.Postgresql

import Application.DTO
import Application.Banking
import Infrastructure.Cache.AppCache
import Test.Cucumber.Runner (StepAction, StepActionParam (ParamDouble))
import Control.Monad.IO.Class (liftIO)

data DepositStepData = DepositStepData
 { depositStepDataIban  :: Text
 , depositStepDataCache :: AppCache
 , depositStepDataConn  :: SqlBackend
 }

-- Given my Giro account has a balance of {double}
givenDepositStep :: StepType
givenDepositStep = Given 
                    (Text "my Giro account has a balance of" 
                      (Param Double StepEnd)) 
givenDeposit :: StepAction DepositStepData
givenDeposit [ParamDouble balance]  = do
  liftIO $ putStrLn "givenDeposit begin"
  iban  <- gets depositStepDataIban
  cache <- gets depositStepDataCache
  conn  <- gets depositStepDataConn

  owner <- liftIO $ createCustomer cache "Jonathan" conn
  void $ liftIO $ createAccount cache owner iban balance "Giro" conn
  liftIO $ putStrLn "givenDeposit end"
givenDeposit _ = fail "Invalid params in givenDeposit"

-- When I deposit {double} into my account
whenDepositStep :: StepType
whenDepositStep = When 
                  (Text "I deposit" 
                    (Param Double 
                      (Text "into my account" StepEnd)))
whenDeposit :: StepAction DepositStepData
whenDeposit [ParamDouble balance] = do
  liftIO $ putStrLn "whenDeposit begin"
  iban  <- gets depositStepDataIban
  cache <- gets depositStepDataCache
  conn  <- gets depositStepDataConn

  ret <- liftIO $ deposit cache iban balance conn
  case ret of
    Nothing -> liftIO $ putStrLn "whenDeposit end"
    (Just e) -> do
      fail $ "whenDeposit failed: could not deposit " ++ show e
whenDeposit _ = fail "Invalid params in whenDeposit"

-- Then I should have a balance of {double} in my account
thenDepositStep :: StepType
thenDepositStep = Then 
                    (Text "I should have a balance of" 
                      (Param Double 
                        (Text "in my account" StepEnd)))
thenDeposit :: StepAction DepositStepData
thenDeposit [ParamDouble expectedBalance] = do
  liftIO $ putStrLn "thenDeposit begin"
  iban  <- gets depositStepDataIban
  cache <- gets depositStepDataCache
  conn  <- gets depositStepDataConn

  ret <- liftIO $ getAccount cache iban conn
  case ret of
    (Left _) -> fail "Account Not Found!"
    (Right a) -> do
      let balance = accountDetailBalance (accountDetails a)
      if (abs (balance - expectedBalance) > 0.01)
        then fail $ "Expected balance " ++ show expectedBalance ++ " but was " ++ show balance
        else liftIO $ putStrLn "thenDeposit end"
thenDeposit _ = fail "Invalid params in thenDeposit"