module BDD.Steps.AccountSteps where

import BDD.Runner
import BDD.Data.Step

import Application.DTO
import Application.Banking
import Infrastructure.Cache.AppCache
import Infrastructure.DB.PgPool

-- Given my Giro account has a balance of {double}
givenGiroAccountBalanceStep :: StepType
givenGiroAccountBalanceStep = Given 
                                (Text "my Giro account has a balance of" 
                                  (Param Double StepEnd)) 
givenGiroAccountBalance :: AppCache
                        -> PgPool
                        -> StepAction
givenGiroAccountBalance cache pool [ParamDouble balance]  = do
  let iban = "AT99 99999 9999999999"
  owner <- createCustomer cache pool "Jonathan"
  _ <- createAccount cache pool owner iban balance "GIRO"
  return ()
givenGiroAccountBalance _ _ _ = Prelude.error "Invalid params in givenGiroAccountBalance"

-- When I deposit {double} into my account
whenDepositBalanceStep :: StepType
whenDepositBalanceStep = When 
                          (Text "I deposit" 
                            (Param Double 
                              (Text "into my account" StepEnd)))
whenDepositBalance :: AppCache
                   -> PgPool
                   -> StepAction
whenDepositBalance cache pool [ParamDouble balance] = do
  let iban = "AT99 99999 9999999999"
  _ <- deposit cache pool iban balance
  return ()
whenDepositBalance _ _ _ = Prelude.error "Invalid params in whenDepositBalance"

-- Then I should have a balance of {double} in my account
thenExpectNewBalanceStep :: StepType
thenExpectNewBalanceStep = Then 
                            (Text "I should have a balance of" 
                              (Param Double 
                                (Text "in my account" StepEnd)))
thenExpectNewBalance :: AppCache
                     -> PgPool
                     -> StepAction
thenExpectNewBalance cache pool [ParamDouble expectedBalance] = do
  let iban = "AT99 99999 9999999999"
  ret <- getAccount cache pool iban
  case ret of
    (Left _) -> Prelude.error "Account Not Found!"
    (Right a) -> do
      let balance = accountDetailBalance (accountDetails a)
      if (abs (balance - expectedBalance) > 0.01)
        then Prelude.error $ "Expected balance " ++ show expectedBalance ++ " but was " ++ show balance
        else return ()
thenExpectNewBalance _ _ _ = Prelude.error "Invalid params in thenExpectNewBalance"