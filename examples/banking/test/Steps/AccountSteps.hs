module Steps.AccountSteps where

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

data AccountStepsData = AccountStepsData
 { accountStepDataIban  :: Text
 , accountStepDataCache :: AppCache
 , accountStepDataConn  :: SqlBackend
 }

-- Given my Giro account has a balance of {double}
givenGiroAccountBalanceStep :: StepType
givenGiroAccountBalanceStep = Given 
                                (Text "my Giro account has a balance of" 
                                  (Param Double StepEnd)) 
givenGiroAccountBalance :: StepAction AccountStepsData
givenGiroAccountBalance [ParamDouble balance]  = do
  liftIO $ putStrLn "givenGiroAccountBalance begin"
  iban  <- gets accountStepDataIban
  cache <- gets accountStepDataCache
  conn  <- gets accountStepDataConn

  owner <- liftIO $ createCustomer cache conn "Jonathan"
  void $ liftIO $ createAccount cache conn owner iban balance "GIRO"
  liftIO $ putStrLn "givenGiroAccountBalance end"
givenGiroAccountBalance _ = fail "Invalid params in givenGiroAccountBalance"

-- When I deposit {double} into my account
whenDepositBalanceStep :: StepType
whenDepositBalanceStep = When 
                          (Text "I deposit" 
                            (Param Double 
                              (Text "into my account" StepEnd)))
whenDepositBalance :: StepAction AccountStepsData
whenDepositBalance [ParamDouble balance] = do
  liftIO $ putStrLn "whenDepositBalance begin"
  iban  <- gets accountStepDataIban
  cache <- gets accountStepDataCache
  conn  <- gets accountStepDataConn

  ret <- liftIO $ deposit cache conn iban balance
  case ret of
    Nothing -> liftIO $ putStrLn "whenDepositBalance end"
    (Just e) -> do
      fail $ "whenDepositBalance failed: could not deposit " ++ show e
whenDepositBalance _ = fail "Invalid params in whenDepositBalance"

-- Then I should have a balance of {double} in my account
thenExpectNewBalanceStep :: StepType
thenExpectNewBalanceStep = Then 
                            (Text "I should have a balance of" 
                              (Param Double 
                                (Text "in my account" StepEnd)))
thenExpectNewBalance :: StepAction AccountStepsData
thenExpectNewBalance [ParamDouble expectedBalance] = do
  liftIO $ putStrLn "thenExpectNewBalance begin"
  iban  <- gets accountStepDataIban
  cache <- gets accountStepDataCache
  conn  <- gets accountStepDataConn

  ret <- liftIO $ getAccount cache conn iban
  case ret of
    (Left _) -> fail "Account Not Found!"
    (Right a) -> do
      let balance = accountDetailBalance (accountDetails a)
      if (abs (balance - expectedBalance) > 0.01)
        then fail $ "Expected balance " ++ show expectedBalance ++ " but was " ++ show balance
        else liftIO $ putStrLn "thenExpectNewBalance end"
thenExpectNewBalance _ = fail "Invalid params in thenExpectNewBalance"