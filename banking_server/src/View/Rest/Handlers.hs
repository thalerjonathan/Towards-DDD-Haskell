module View.Rest.Handlers
  ( handleAllCustomers
  , handleCustomer
  , handleAccount
  , handleDeposit
  , handleWithdraw
  , handleTransfer
  , handleSwagger
  ) where

import           Control.Monad.Except
import           Data.Swagger
import qualified Data.Text                     as T
import           Servant

import           Application.DTO
import           View.Rest.Api

import           Application.BankingAnemic
import           Database.Persist.Postgresql
import           Infrastructure.Cache.AppCache
import           Infrastructure.DB.Pool        as Pool

handleAllCustomers :: AppCache
                   -> DbPool
                   -> Handler [CustomerDetailsDTO]
handleAllCustomers cache p =
  liftIO $ Pool.runWithTX p (getAllCustomers cache)

handleCustomer :: AppCache
               -> DbPool
               -> T.Text
               -> Handler CustomerDTO
handleCustomer cache p customerId = do
  ret <- liftIO $ Pool.runWithTX p (runExceptT . getCustomer cache customerId)
  case ret of
    (Left _)     -> throwError err404
    (Right cust) -> return cust

handleAccount :: AppCache
              -> DbPool
              -> T.Text
              -> Handler AccountDTO
handleAccount cache p iban = do
  ret <- liftIO $ Pool.runWithTX p (runExceptT . getAccount cache iban)
  case ret of
    (Left _)  -> throwError err404
    (Right a) -> return a

handleDeposit :: AppCache
              -> DbPool
              -> T.Text
              -> Double
              -> Handler (Either T.Text TXLineDTO)
handleDeposit cache p iban amount =
  performAccountTx p (runExceptT . deposit cache iban amount)

handleWithdraw :: AppCache
               -> DbPool
               -> T.Text
               -> Double
               -> Handler (Either T.Text TXLineDTO)
handleWithdraw cache p iban amount =
  performAccountTx p (runExceptT . withdraw cache iban amount)

handleTransfer :: AppCache
               -> DbPool
               -> T.Text
               -> T.Text
               -> Double
               -> T.Text
               -> Handler (Either T.Text TXLineDTO)
handleTransfer cache p fromIban toIban amount reference =
  performAccountTx p (runExceptT . transferEventual cache fromIban toIban amount reference)

handleSwagger :: Handler Swagger
handleSwagger = return bankingSwagger


performAccountTx :: DbPool
                 -> (SqlBackend -> IO (Either Exception TXLineDTO))
                 -> Handler (Either T.Text TXLineDTO)
performAccountTx p act = do
  ret <- liftIO $ Pool.runWithTX p act
  case ret of
    (Left (InvalidAccountOperation str)) -> return $ Left str
    (Left _)                             -> throwError err404
    (Right tx)                           -> return $ Right tx
