{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.BDD.Steps.Transferring where

import           Application.Anemic.Banking
import           Application.DTO
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State           (gets, put)
import           Data.Maybe
import           Data.Text
import           Database.Persist.Postgresql
import           Infrastructure.Cache.AppCache
import           Test.Cucumber
import           Test.Tasty.HUnit

data StepData = StepData
 { stepDataOwner :: Maybe Text
 , stepDataCache :: AppCache
 , stepDataConn  :: SqlBackend
 , stepDataEx    :: Maybe Exception
 }

givenAccount :: (StepType, StepAction StepData)
givenAccount = (

    [given_|a {word} account with Iban {string} and a balance of {double}|]

  , \case
    [ParamWord accountType, ParamString iban, ParamDouble balance] -> do
      _cache <- gets stepDataCache
      conn  <- gets stepDataConn

      owner <- liftIO $ createCustomer "Testcustomer" conn
      void $ liftIO $ runExceptT $ createAccount owner (pack iban) balance (pack accountType) conn
      put $ StepData (Just owner) _cache conn Nothing
    _ -> fail "Invalid params in givenAccountBalance"
  )

givenAccountSameCustomer :: (StepType, StepAction StepData)
givenAccountSameCustomer = (

    [given_|a {word} account of the same customer with Iban {string} and a balance of {double}|]

  , \case
    [ParamWord accountType, ParamString iban, ParamDouble balance] -> do
      
      _cache <- gets stepDataCache
      conn  <- gets stepDataConn
      owner <- fromJust <$> gets stepDataOwner

      void $ liftIO $ runExceptT $ createAccount owner (pack iban) balance (pack accountType) conn
    _ -> fail "Invalid params in givenAccountBalance"
  )

whenTransferring :: (StepType, StepAction StepData)
whenTransferring = (

    [when_|Transferring {double} from Iban {string} to Iban {string}|]

  , \case
    [ParamDouble amount, ParamString fromIban, ParamString toIban] -> do
      _cache <- gets stepDataCache
      conn  <- gets stepDataConn

      ret <- liftIO $ runExceptT $ transferTransactional (pack fromIban) (pack toIban) amount "Testtransfer" conn
      case ret of
        (Right _)  -> return ()
        (Left err) -> put $ StepData Nothing _cache conn (Just err)
    _ -> fail "Invalid params in whenDeposit"
  )

thenExpectBalance :: (StepType, StepAction StepData)
thenExpectBalance = (

    [then_|There should be a balance of {double} in the account with Iban {string}|]

  , \case
    [ParamDouble expectedBalance, ParamString iban] -> do
      _cache <- gets stepDataCache
      conn  <- gets stepDataConn

      ret <- liftIO $ runExceptT $ getAccount (pack iban) conn
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
