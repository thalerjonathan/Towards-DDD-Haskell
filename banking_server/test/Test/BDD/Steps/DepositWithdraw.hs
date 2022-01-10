{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.BDD.Steps.DepositWithdraw where

import           Application.Anemic.Banking
import           Application.DTO
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State           (gets, put)
import           Data.Text
import           Database.Persist.Postgresql
import           Infrastructure.Cache.AppCache
import           Test.Cucumber.Data.Step
import           Test.Tasty.HUnit

import           Test.Cucumber

data StepData = StepData
 { stepDataIban  :: Text
 , stepDataCache :: AppCache
 , stepDataConn  :: SqlBackend
 , stepDataEx    :: Maybe Exception
 }

givenAccount :: (StepType, StepAction StepData)
givenAccount = (

    [given_|my {word} account has a balance of {double}|]

  , \case
    [ParamWord accountType, ParamDouble balance] -> do
      iban  <- gets stepDataIban
      _cache <- gets stepDataCache
      conn  <- gets stepDataConn

      owner <- liftIO $ createCustomer "Jonathan Thaler" conn
      void $ liftIO $ runExceptT $ createAccount owner iban balance (pack accountType) conn
    _ -> fail "Invalid params in givenAccountBalance"
  )

whenDeposit :: (StepType, StepAction StepData)
whenDeposit = (

    [when_|I deposit {double} into my account|]

  , \case
    [ParamDouble balance] -> do
      iban  <- gets stepDataIban
      _cache <- gets stepDataCache
      conn  <- gets stepDataConn

      ret <- liftIO $ runExceptT $ deposit iban balance conn
      case ret of
        (Right _)  -> return ()
        (Left err) -> put $ StepData iban _cache conn (Just err)
    _ -> fail "Invalid params in whenDeposit"
  )

whenWithdraw :: (StepType, StepAction StepData)
whenWithdraw = (

    [when_|I withdraw {double} from my account|]

  , \case
    [ParamDouble balance] -> do
      iban  <- gets stepDataIban
      _cache <- gets stepDataCache
      conn  <- gets stepDataConn

      ret <- liftIO $ runExceptT $ withdraw iban balance conn
      case ret of
        (Right _)  -> return ()
        (Left err) -> put $ StepData iban _cache conn (Just err)
    _ -> fail "Invalid params in whenWithdraw"
  )

thenExpectBalance :: (StepType, StepAction StepData)
thenExpectBalance = (

    [then_|I should have a balance of {double} in my account|]

  , \case
    [ParamDouble expectedBalance] -> do
      iban  <- gets stepDataIban
      _cache <- gets stepDataCache
      conn  <- gets stepDataConn

      ret <- liftIO $ runExceptT $ getAccount iban conn
      case ret of
        (Left _) -> fail "Account Not Found!"
        (Right a) -> do
          let balance = accountDetailBalance (accountDetails a)
          if (abs (balance - expectedBalance) > 0.01)
            then fail $ "Expected balance " ++ show expectedBalance ++ " but was " ++ show balance
            else return ()
    _ -> fail "Invalid params in thenExpectBalance"
  )

thenExpectError :: (StepType, StepAction StepData)
thenExpectError = (

    [then_|I expect the error {string}|]

  , \case
    [ParamString expectedErr] -> do
      mErr <- gets stepDataEx
      case mErr of
        Nothing -> fail "Expected InvalidAccountOperation but got none!"
        Just (InvalidAccountOperation err) -> liftIO $ assertEqual "" (pack expectedErr) err
        _ -> fail "Expected InvalidAccountOperation but was different exception!"
    _ -> fail "Invalid params in thenExpectError"
  )
