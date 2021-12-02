{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Infrastructure.DB.Banking
  ( Entity (..)
  , CustomerEntity (..)
  , AccountEntity (..)
  , TXLineEntity (..)
  , CustomerEntityId
  , AccountEntityId
  , TXLineEntityId
  , AccountEntityType (..)
  , PersistedEventEntity (..)

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

import           Control.Monad.Reader
import           Data.Maybe
import           Data.Text
import           Data.Time
import           Database.Persist.Quasi
import           Database.Persist.Sql
import           Database.Persist.TH

data AccountEntityType
  = Giro
  | Savings
  deriving (Eq, Show, Read)
derivePersistField "AccountEntityType"

share [mkPersist sqlSettings]
    $(persistFileWith lowerCaseSettings "db/banking.persistentmodels")

insertEvent :: PersistedEventEntity -> SqlBackend -> IO (Key PersistedEventEntity)
insertEvent pe = executeActionWithoutTX (insert pe)

insertCustomer :: CustomerEntity -> SqlBackend -> IO (Key CustomerEntity)
insertCustomer cust = executeActionWithoutTX (insert cust)

insertAccount :: AccountEntity -> SqlBackend -> IO (Key AccountEntity)
insertAccount a = executeActionWithoutTX (insert a)

allCustomers :: SqlBackend -> IO [Entity CustomerEntity]
allCustomers = executeActionWithoutTX (selectList [] [])

nextEvent :: SqlBackend -> IO (Maybe (Entity PersistedEventEntity))
nextEvent = executeActionWithoutTX act
  where
    act = listToMaybe <$>
            selectList [ PersistedEventEntityProcessed ==. False
                      , PersistedEventEntityFailed ==. False ] [Desc PersistedEventEntityCreated, LimitTo 1]

markEventFailed :: PersistedEventEntityId -> Text -> SqlBackend -> IO ()
markEventFailed pid err = executeActionWithoutTX act
  where
    act = update pid [ PersistedEventEntityFailed =. True
                     , PersistedEventEntityFailedError =. err ]

markEventProcessed :: PersistedEventEntityId -> SqlBackend -> IO ()
markEventProcessed pid = executeActionWithoutTX act
  where
    act = update pid [ PersistedEventEntityProcessed =. True ]

customerById :: Text -> SqlBackend -> IO (Maybe (Entity CustomerEntity))
customerById domainId = executeActionWithoutTX act
  where
    act = listToMaybe <$>
            selectList [CustomerEntityDomainId ==. domainId] [LimitTo 1]

customerByCustomerId :: CustomerEntityId -> SqlBackend -> IO (Maybe CustomerEntity)
customerByCustomerId cid = executeActionWithoutTX (get cid)

accountByIban :: Text -> SqlBackend -> IO (Maybe (Entity AccountEntity))
accountByIban iban = executeActionWithoutTX act
  where
    act = listToMaybe <$>
            selectList [AccountEntityIban ==. iban] [LimitTo 1]

accountsOfCustomer :: CustomerEntityId -> SqlBackend -> IO [Entity AccountEntity]
accountsOfCustomer cid = executeActionWithoutTX act
  where
    act = selectList [AccountEntityOwner ==. cid] []

txLinesOfAccount :: AccountEntityId -> SqlBackend -> IO [Entity TXLineEntity]
txLinesOfAccount aid = executeActionWithoutTX act
  where
    act = selectList [TXLineEntityAccount ==. aid] [] 

updateAccountBalance :: AccountEntityId -> Double -> SqlBackend -> IO ()
updateAccountBalance aid b = executeActionWithoutTX act
  where
    act = update aid [AccountEntityBalance =. b]

insertTXLine :: TXLineEntity -> SqlBackend -> IO (Key TXLineEntity)
insertTXLine tx = executeActionWithoutTX (insert tx)

executeActionWithoutTX :: ReaderT SqlBackend IO a -> SqlBackend -> IO a
executeActionWithoutTX act = liftIO . runReaderT act
