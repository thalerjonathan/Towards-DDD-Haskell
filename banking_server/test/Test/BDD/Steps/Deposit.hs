{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.BDD.Steps.Deposit where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State           (gets, put)
import           Data.Text
import           Database.Persist.Postgresql
import           Test.Cucumber.Data.Step
import           Test.Tasty.HUnit

import           Application.BankingAnemic
import           Application.DTO
import           Infrastructure.Cache.AppCache

import           Test.Cucumber

data DepositStepData = DepositStepData
 { depositStepDataIban  :: Text
 , depositStepDataCache :: AppCache
 , depositStepDataConn  :: SqlBackend
 , depositException     :: Maybe Exception
 }

givenAccount :: (StepType, StepAction DepositStepData)
givenAccount = (

    [given_|my {word} account has a balance of {double}|]

  , \case
    [ParamWord accountType, ParamDouble balance] -> do
      iban  <- gets depositStepDataIban
      _cache <- gets depositStepDataCache
      conn  <- gets depositStepDataConn

      owner <- liftIO $ createCustomer "Jonathan Thaler" conn
      void $ liftIO $ runExceptT $ createAccount owner iban balance (pack accountType) conn
    _ -> fail "Invalid params in givenAccountBalance"
  )

whenDeposit :: (StepType, StepAction DepositStepData)
whenDeposit = (

    [when_|I deposit {double} into my account|]

  , \case
    [ParamDouble balance] -> do
      iban  <- gets depositStepDataIban
      _cache <- gets depositStepDataCache
      conn  <- gets depositStepDataConn

      ret <- liftIO $ runExceptT $ deposit iban balance conn
      case ret of
        (Right _)  -> return ()
        (Left err) -> put $ DepositStepData iban _cache conn (Just err)
    _ -> fail "Invalid params in whenDeposit"
  )

thenDeposit :: (StepType, StepAction DepositStepData)
thenDeposit = (

    [then_|I should have a balance of {double} in my account|]

  , \case
    [ParamDouble expectedBalance] -> do
      iban  <- gets depositStepDataIban
      _cache <- gets depositStepDataCache
      conn  <- gets depositStepDataConn

      ret <- liftIO $ runExceptT $ getAccount iban conn
      case ret of
        (Left _) -> fail "Account Not Found!"
        (Right a) -> do
          let balance = accountDetailBalance (accountDetails a)
          if (abs (balance - expectedBalance) > 0.01)
            then fail $ "Expected balance " ++ show expectedBalance ++ " but was " ++ show balance
            else return ()
    _ -> fail "Invalid params in thenDeposit"
  )

thenExpectError :: (StepType, StepAction DepositStepData)
thenExpectError = (

    [then_|I expect the error {string}|]

  , \case
    [ParamString expectedErr] -> do
      mErr <- gets depositException
      case mErr of
        Nothing -> fail "Expected InvalidAccountOperation but got none!"
        Just (InvalidAccountOperation err) -> liftIO $ assertEqual "" (pack expectedErr) err
        _ -> fail "Expected InvalidAccountOperation but was different exception!"
    _ -> fail "Invalid params in thenExpectError"
  )
