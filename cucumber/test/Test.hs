module Main where

import           Text.Megaparsec               (parse)

import           Steps.AccountSteps            (AccountStepsData,
                                                givenAccountBalance,
                                                thenExpectError,
                                                thenExpectNewBalance,
                                                whenDepositBalance)
import           Test.Cucumber                 (runFeature)
import           Test.Cucumber.Data.Step       (StepType)
import           Test.Cucumber.Parsing.Gherkin (parseGherkin)
import           Test.Cucumber.Runner          (StepAction)

testFeatureSteps :: [(StepType, StepAction AccountStepsData)]
testFeatureSteps = [ givenAccountBalance
                   , whenDepositBalance
                   , thenExpectNewBalance
                   , thenExpectError
                  ]

main :: IO ()
main = do
  s <- readFile "test/features/account/Depositing.feature"
  let ret = parse parseGherkin "" s
  case ret of
    (Left e) -> print e
    (Right feature) -> do
      runFeature
        feature
        (\scenarioAction -> do
          scenarioAction Nothing
        ) [ givenAccountBalance
            , whenDepositBalance
            , thenExpectNewBalance
            , thenExpectError
          ]
