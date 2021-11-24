module Main where

import Text.Megaparsec hiding (State)

import BDD.Parsing.Feature

main :: IO ()
main = do
  let _scenario1 = "Scenario: Deposit money into a Giro account\n" ++
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
                  "And There should be a balance of 500.0 in the account with Iban 'AT98 98765 09876543210'"

  --s <- readFile "test/resources/features/Depositing.feature"
  --print s
  let ret = parse parseFeature "" _scenario2
  case ret of
    (Left e) -> print e
    (Right f) -> do
      let steps =
                  [ ("Given my Giro account has a balance of 1234.56", givenGiroAccountBalance 1234.56)
                  , ("When I deposit 567.89 into my account", whenDepositBalance 567.89)
                  , ("Then I should have a balance of 1802.45 in my account", thenExpectNewBalance 1802.45)
                  ]

      let beforeScenario = print "Before Scenario"
      let afterScenario = print "After Scenario"
      runFeature f beforeScenario afterScenario steps

runFeature :: Feature 
           -> IO ()
           -> IO ()
           -> [(String, IO ())] 
           -> IO ()
runFeature _f _beforeScenario _afterScenario _steps = do
  -- TODO: run each scenario in separate transaction and roll back at the end to avoid persistent changes to DB. Implement through BeforeScenario
  return ();

-- Given my Giro account has a balance of {double}
givenGiroAccountBalance :: Double -> IO ()
givenGiroAccountBalance _balance = do
  -- TODO: create giro account
  undefined

-- When I deposit {double} into my account
whenDepositBalance :: Double -> IO ()
whenDepositBalance _balance = do
  -- TODO: load giro account
  -- TODO: deposit
  undefined

-- Then I should have a balance of {double} in my account
thenExpectNewBalance :: Double -> IO ()
thenExpectNewBalance _balance = do
  -- TODO: load giro account
  -- TODO: check balance
  undefined