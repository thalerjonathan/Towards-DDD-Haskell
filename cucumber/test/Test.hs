module Main where

import Text.Megaparsec hiding (State)

import Test.Cucumber.Parsing.Gherkin
import Steps.AccountSteps
import Test.Cucumber
import Test.Cucumber.Data.Step (StepType)
import Test.Cucumber.Runner (StepAction)

testFeature :: String
testFeature = "Feature: Depositing money into accounts\n" ++
                  "In order to manage my money more efficiently\n" ++
                  "As a bank client\n" ++
                  "I want to deposit money into my accounts whenever I need to.\n" ++
                "Scenario: Deposit money into a Giro account\n" ++
                  "Given my Giro account has a balance of 1234.56\n" ++
                  "When I deposit 567.89 into my account\n" ++ 
                  "Then I should have a balance of 1802.45 in my account\n"

testFeatureSteps :: [(StepType, StepAction AccountStepsData)]
testFeatureSteps = [ (givenGiroAccountBalanceStepGenerated,  -- givenGiroAccountBalanceStep
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
  let ret = parse parseGherkin "" testFeature
  case ret of
    (Left e) -> print e
    (Right feature) -> do
      runFeature 
        feature
        (\scenarioAction -> do
          putStrLn "Before Scenario"
          scenarioAction Nothing
          putStrLn "After Scenario"
        ) testFeatureSteps