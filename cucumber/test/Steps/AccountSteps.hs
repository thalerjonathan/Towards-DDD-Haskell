{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Steps.AccountSteps where

import Test.Cucumber.Runner
import Test.Cucumber.Data.Step
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)

-- import Test.Cucumber.Generator.StepDefinition ( given )

type AccountStepsData = Maybe Double

-- givenGiroAccountBalanceStepGenerated :: StepType
-- givenGiroAccountBalanceStepGenerated = [given|my Giro account has a balance of {double}|]

-- Given my Giro account has a balance of {double}
givenGiroAccountBalanceStep :: StepType
givenGiroAccountBalanceStep = Given 
                                (Text "my Giro account has a balance of" 
                                  (Param Double StepEnd))

givenGiroAccountBalance :: StepAction AccountStepsData
givenGiroAccountBalance [ParamDouble initialBalance]  = do
  liftIO $ putStrLn "givenGiroAccountBalance begin"
  put (Just initialBalance)
  liftIO $ putStrLn "givenGiroAccountBalance end"
givenGiroAccountBalance _ = fail "Invalid params in givenGiroAccountBalance"

-- When I deposit {double} into my account
whenDepositBalanceStep :: StepType
whenDepositBalanceStep = When 
                          (Text "I deposit" 
                            (Param Double 
                              (Text "into my account" StepEnd)))
whenDepositBalance :: StepAction AccountStepsData
whenDepositBalance [ParamDouble amount] = do
  liftIO $ putStrLn "whenDepositBalance begin"
  ret <- get
  case ret of
    Nothing -> fail "Account not found!"
    Just balance -> do
      put (Just $ balance + amount)
      liftIO $ putStrLn "whenDepositBalance end"
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
  ret <- get
  case ret of
    Nothing -> fail "Account not found!"
    Just balance -> do
      if abs (balance - expectedBalance) > 0.01
        then fail $ "Expected balance " ++ show expectedBalance ++ " but was " ++ show balance
        else liftIO $ putStrLn "thenExpectNewBalance end"
thenExpectNewBalance _ = fail "Invalid params in thenExpectNewBalance"