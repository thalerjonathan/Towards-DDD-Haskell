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
module Infrastructure.DB.Banking 
  ( Entity (..)
  , Customer (..)
  , Account (..)
  , TXLine (..)
  , CustomerId
  , AccountId
  , TXLineId
  , AccountType (..)
  
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

data AccountType 
  = Giro
  | Savings
  deriving (Eq, Show, Read)
derivePersistField "AccountType"

share [mkPersist sqlSettings]
    $(persistFileWith lowerCaseSettings "db/banking.persistentmodels")

beginTX :: SqlBackend -> IO ()
beginTX = executeActionWithoutTX (rawExecute "BEGIN" [])

rollbackTX :: SqlBackend -> IO ()
rollbackTX = executeActionWithoutTX (rawExecute "ROLLBACK" [])

commitTX :: SqlBackend -> IO ()
commitTX = executeActionWithoutTX (rawExecute "COMMIT" [])

insertCustomer :: Customer -> SqlBackend -> IO (Key Customer)
insertCustomer cust = executeActionWithoutTX (insert cust)

insertAccount :: Account -> SqlBackend -> IO (Key Account)
insertAccount a = executeActionWithoutTX (insert a)

allCustomers :: SqlBackend -> IO [Entity Customer]
allCustomers = executeActionWithoutTX (selectList [] [])

customerById :: Text -> SqlBackend -> IO (Maybe (Entity Customer))
customerById domainId = executeActionWithoutTX act
  where
    act = do 
      ret <- selectList [CustomerDomainId ==. domainId] [LimitTo 1]
      if Prelude.null ret
        then return Nothing
        else return $ Just (Prelude.head ret)

accountByIban :: Text -> SqlBackend -> IO (Maybe (Entity Account))
accountByIban iban = executeActionWithoutTX act
  where
    act = do 
      ret <- selectList [AccountIban ==. iban] [LimitTo 1]
      if Prelude.null ret
        then return Nothing
        else return $ Just (Prelude.head ret)

accountsOfCustomer :: CustomerId -> SqlBackend -> IO [Entity Account]
accountsOfCustomer cid = executeActionWithoutTX act
  where
    act = selectList [AccountOwner ==. cid] [] --[LimitTo 1]

txLinesOfAccount :: AccountId -> SqlBackend -> IO [Entity TXLine]
txLinesOfAccount aid = executeActionWithoutTX act
  where
    act = selectList [TXLineAccount ==. aid] [] --[LimitTo 1]

updateAccountBalance :: AccountId -> Double -> SqlBackend -> IO ()
updateAccountBalance aid b = executeActionWithoutTX act
  where
    act = update aid [AccountBalance =. b]

insertTXLine :: TXLine -> SqlBackend -> IO (Key TXLine)
insertTXLine tx = executeActionWithoutTX act
  where
    act = insert tx

executeActionWithoutTX :: ReaderT SqlBackend IO a -> SqlBackend -> IO a
executeActionWithoutTX act = liftIO . runReaderT act