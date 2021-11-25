module BDD.Steps.AccountSteps where

import BDD.Runner
import BDD.Data.Step

import Database.Persist.Postgresql

import Application.DTO
import Application.Banking
import Infrastructure.Cache.AppCache

-- Given my Giro account has a balance of {double}
givenGiroAccountBalanceStep :: StepType
givenGiroAccountBalanceStep = Given 
                                (Text "my Giro account has a balance of" 
                                  (Param Double StepEnd)) 
givenGiroAccountBalance :: AppCache
                        -> StepAction SqlBackend
givenGiroAccountBalance cache conn [ParamDouble balance]  = do
  let iban = "AT99 99999 9999999999"
  putStrLn "givenGiroAccountBalance begin"
  owner <- createCustomer cache conn "Jonathan"
  _ <- createAccount cache conn owner iban balance "GIRO"
  putStrLn "givenGiroAccountBalance end"
  return ()
givenGiroAccountBalance _ _ _ = Prelude.error "Invalid params in givenGiroAccountBalance"

-- When I deposit {double} into my account
whenDepositBalanceStep :: StepType
whenDepositBalanceStep = When 
                          (Text "I deposit" 
                            (Param Double 
                              (Text "into my account" StepEnd)))
whenDepositBalance :: AppCache
                   -> StepAction SqlBackend
whenDepositBalance cache conn [ParamDouble balance] = do
  let iban = "AT99 99999 9999999999"
  _ <- deposit cache conn iban balance
  return ()
whenDepositBalance _ _ _ = Prelude.error "Invalid params in whenDepositBalance"

-- Then I should have a balance of {double} in my account
thenExpectNewBalanceStep :: StepType
thenExpectNewBalanceStep = Then 
                            (Text "I should have a balance of" 
                              (Param Double 
                                (Text "in my account" StepEnd)))
thenExpectNewBalance :: AppCache
                     -> StepAction SqlBackend
thenExpectNewBalance cache conn [ParamDouble expectedBalance] = do
  let iban = "AT99 99999 9999999999"
  ret <- getAccount cache conn iban
  case ret of
    (Left _) -> Prelude.error "Account Not Found!"
    (Right a) -> do
      let balance = accountDetailBalance (accountDetails a)
      if (abs (balance - expectedBalance) > 0.01)
        then Prelude.error $ "Expected balance " ++ show expectedBalance ++ " but was " ++ show balance
        else return ()
thenExpectNewBalance _ _ _ = Prelude.error "Invalid params in thenExpectNewBalance"