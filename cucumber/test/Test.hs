module Main where

import Text.Megaparsec hiding (State)

import BDD.Parsing.Gherkin
import BDD.Steps.AccountSteps
import BDD.Runner

import Control.Monad.Reader
import Control.Monad.Except

testFeature :: String
testFeature = "Feature: Depositing money into accounts\n" ++
                  "In order to manage my money more efficiently\n" ++
                  "As a bank client\n" ++
                  "I want to deposit money into my accounts whenever I need to.\n" ++
                "Scenario: Deposit money into a Giro account\n" ++
                  "Given my Giro account has a balance of 1234.56\n" ++
                  "When I deposit 567.89 into my account\n" ++ 
                  "Then I should have a balance of 1802.45 in my account\n"

testFeatureSteps = [ (givenGiroAccountBalanceStep, 
                      givenGiroAccountBalance cache)
                  , 
                    (whenDepositBalanceStep,
                      whenDepositBalance cache)
                  , 
                    ( thenExpectNewBalanceStep,
                      thenExpectNewBalance cache)
                  ]

main :: IO ()
main = do
  --s <- readFile "test/resources/features/Depositing.feature"
  let ret = parse parseGherkin "" testFeature
  case ret of
    (Left e) -> print e
    (Right feat) -> do
      runFeature 
        feat
        (\scenarioAction -> do
          putStrLn "Before Scenario"
          
          runWithConnection dbPool $ \conn -> do 
            putStrLn "BEGIN TX"
            beginTX conn
            _ <- scenarioAction conn
            putStrLn "ROLLBACK TX"
            liftIO $ runReaderT transactionUndo conn

          putStrLn "After Scenario"
        ) testFeatureSteps