{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}
module Steps.AccountSteps where

import           Control.Monad.State                    (get, put)
import           Test.Cucumber.Data.Step                (StepType (..))
import           Test.Cucumber.Runner                   (StepAction,
                                                         StepActionParam (ParamDouble, ParamString, ParamWord))

import           Control.Monad                          (when)
import           Test.Cucumber.Generator.StepDefinition (given_, then_, when_)

type AccountStepsData = Maybe (String, Double, Maybe String)

givenAccountBalance :: (StepType, StepAction AccountStepsData)
givenAccountBalance = (

    [given_|my {word} account has a balance of {double}|]

  , \case
    [ParamWord accountType, ParamDouble initialBalance] -> do
      put $ Just (accountType, initialBalance, Nothing)
    _ -> fail "Invalid params in givenGiroAccountBalance"
  )

whenDepositBalance :: (StepType, StepAction AccountStepsData)
whenDepositBalance = (

    [when_|I deposit {double} into my account|]

  , \case
    [ParamDouble amount] -> do
      ret <- get
      case ret of
        Nothing -> fail "Account State not found!"
        Just (at, bal, _) -> do
          if at == "Savings"
            then do put $ Just (at, bal, Just "Cannot deposit money directly into Savings Account! Use transfer of money from a Giro Account of the same customer.")
            else do put $ Just (at, bal + amount, Nothing)
    _ -> fail "Invalid params in whenDepositBalance"
  )

thenExpectNewBalance :: (StepType, StepAction AccountStepsData)
thenExpectNewBalance = (

    [then_|I should have a balance of {double} in my account|]

  , \case
    [ParamDouble expectedBalance] -> do
      ret <- get
      case ret of
        Nothing -> fail "Account State not found!"
        Just (_, bal, _) -> do
          when (abs (bal - expectedBalance) > 0.01)
            (fail $ "Expected balance " ++ show expectedBalance ++ " but was " ++ show bal)
    _ -> fail "Invalid params in thenExpectNewBalance"
  )

thenExpectError :: (StepType, StepAction AccountStepsData)
thenExpectError = (

    [then_|I expect the error {string}|]

  , \case
    [ParamString expectedErr] -> do
      ret <- get
      case ret of
        Nothing -> fail "Account State not found!"
        Just (_, _, Just actualErr) -> do
          when (expectedErr /= actualErr)
              (fail $ "Expected Error '" ++ expectedErr ++ "' but was '" ++ actualErr ++ "'")
        _ -> fail "Expected Error but there was none."
    _ -> fail "Invalid params in thenExpectError"
  )
