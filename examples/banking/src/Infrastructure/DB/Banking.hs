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
  , PersistedEvent (..)

  , insertCustomer
  , insertAccount
  , insertEvent

  , nextEvent
  , markEventFailed
  , markEventProcessed

  , allCustomers
  , customerById
  , customerByCustomerId
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

insertEvent :: PersistedEvent -> SqlBackend -> IO (Key PersistedEvent)
insertEvent pe = executeActionWithoutTX (insert pe)

insertCustomer :: Customer -> SqlBackend -> IO (Key Customer)
insertCustomer cust = executeActionWithoutTX (insert cust)

insertAccount :: Account -> SqlBackend -> IO (Key Account)
insertAccount a = executeActionWithoutTX (insert a)

allCustomers :: SqlBackend -> IO [Entity Customer]
allCustomers = executeActionWithoutTX (selectList [] [])

nextEvent :: SqlBackend -> IO (Maybe (Entity PersistedEvent))
nextEvent = executeActionWithoutTX act
  where
    act = do
      ret <- selectList [ PersistedEventProcessed ==. False
                        , PersistedEventFailed ==. False ] [Desc PersistedEventCreated, LimitTo 1]
      if Prelude.null ret
        then return Nothing
        else return $ Just (Prelude.head ret)

markEventFailed :: PersistedEventId -> Text -> SqlBackend -> IO ()
markEventFailed pid err = executeActionWithoutTX act
  where
    act = update pid [ PersistedEventFailed =. True
                     , PersistedEventFailedError =. err ]

markEventProcessed :: PersistedEventId -> SqlBackend -> IO ()
markEventProcessed pid = executeActionWithoutTX act
  where
    act = update pid [ PersistedEventProcessed =. True ]

customerById :: Text -> SqlBackend -> IO (Maybe (Entity Customer))
customerById domainId = executeActionWithoutTX act
  where
    act = do 
      ret <- selectList [CustomerDomainId ==. domainId] [LimitTo 1]
      if Prelude.null ret
        then return Nothing
        else return $ Just (Prelude.head ret)

customerByCustomerId :: CustomerId -> SqlBackend -> IO (Maybe Customer)
customerByCustomerId cid = executeActionWithoutTX (get cid)

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