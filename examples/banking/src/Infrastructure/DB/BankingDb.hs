{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}

module Infrastructure.DB.BankingDb 
  ( Entity (..)
  , Customer (..)
  , Account (..)
  , TXLine (..)
  , CustomerId
  , AccountId
  , TXLineId

  , runNoTX
  , runWithTX

  , beginTX
  , rollbackTX
  , commitTX

  , insertCustomer
  , insertAccount

  , allCustomers
  , customerById
  , accountByIban
  , accountsOfCustomer
  , txLinesOfAccount
  , updateAccountBalance
  , insertTXLine
  ) where

import Database.Persist.Sql
import Database.Persist.Quasi
import Database.Persist.TH
import Data.Text
import Data.Time
import Control.Monad.Reader
import Infrastructure.DB.PgPool

share [mkPersist sqlSettings]
    $(persistFileWith lowerCaseSettings "db/banking.persistentmodels")

runNoTX :: PgPool -> (SqlBackend -> IO a) -> IO a
runNoTX p act = runSqlPoolNoTransaction (do
  conn <- ask
  liftIO $ act conn) (getPool p) Nothing -- (Just ReadUncommitted)

runWithTX :: PgPool -> (SqlBackend -> IO a) -> IO a
runWithTX p act = runSqlPool (do
  conn <- ask
  liftIO $ act conn) (getPool p)

beginTX :: SqlBackend -> IO ()
beginTX conn = executeActionWithoutTX (rawExecute "BEGIN" []) conn

rollbackTX :: SqlBackend -> IO ()
rollbackTX conn = executeActionWithoutTX (rawExecute "ROLLBACK" []) conn

commitTX :: SqlBackend -> IO ()
commitTX conn = executeActionWithoutTX (rawExecute "COMMIT" []) conn

executeActionWithoutTX :: ReaderT SqlBackend IO a -> SqlBackend -> IO a
executeActionWithoutTX act = liftIO . runReaderT act

insertCustomer :: SqlBackend -> Customer -> IO (Key Customer)
insertCustomer conn cust = executeActionWithoutTX (insert cust) conn

insertAccount :: SqlBackend -> Account -> IO (Key Account)
insertAccount conn a = executeActionWithoutTX (insert a) conn

allCustomers :: SqlBackend -> IO [Entity Customer]
allCustomers conn = executeActionWithoutTX (selectList [] []) conn

customerById :: SqlBackend -> Text -> IO (Maybe (Entity Customer))
customerById conn domainId = executeActionWithoutTX act conn
  where
    act = do 
      ret <- selectList [CustomerDomainId ==. domainId] [LimitTo 1]
      if Prelude.null ret
        then return Nothing
        else return $ Just (Prelude.head ret)

accountByIban :: SqlBackend -> Text -> IO (Maybe (Entity Account))
accountByIban conn iban = executeActionWithoutTX act conn
  where
    act = do 
      ret <- selectList [AccountIban ==. iban] [LimitTo 1]
      if Prelude.null ret
        then return Nothing
        else return $ Just (Prelude.head ret)

accountsOfCustomer :: SqlBackend -> CustomerId -> IO [Entity Account]
accountsOfCustomer conn cid = executeActionWithoutTX act conn
  where
    act = selectList [AccountOwner ==. cid] [] --[LimitTo 1]

txLinesOfAccount :: SqlBackend -> AccountId -> IO [Entity TXLine]
txLinesOfAccount conn aid = executeActionWithoutTX act conn
  where
    act = selectList [TXLineAccount ==. aid] [] --[LimitTo 1]

updateAccountBalance :: SqlBackend -> AccountId -> Double -> IO ()
updateAccountBalance conn aid b = executeActionWithoutTX act conn
  where
    act = update aid [AccountBalance =. b]

insertTXLine :: SqlBackend -> TXLine -> IO (Key TXLine)
insertTXLine conn tx = executeActionWithoutTX act conn
  where
    act = insert tx