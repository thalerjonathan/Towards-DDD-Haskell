{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
module Application.Layer where

import           Control.Monad.Free.Church

import           Application.DomainEvents
import           Data.Text                     as T
import           Data.Time.Clock
import           Data.UUID
import           Data.UUID.V4                  (nextRandom)
import           Database.Persist.Sql
import           Domain.Account.Api
import           Domain.Account.Repository
import           Domain.Customer.Customer
import           Domain.Customer.Repository
import           Infrastructure.Cache.AppCache
import           Infrastructure.DB.Banking     as DB

data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving Show

data Repository a
  = AccountRepo (AccountRepoProgram a)
  | CustomerRepo (CustomerRepoProgram a)

data Aggregate a
  = CustomerAggregate (CustomerProgram a)
  | AccountAggregate (AccountProgram a)

data ApplicationLayerLang a
  = forall b. RunRepo (Repository b) (b -> a)
  | forall b. RunAggregate (Aggregate b) (b -> a)
  | PersistDomainEvent DomainEvent a
  | Logging LogLevel T.Text a
  | NextUUID (UUID -> a)

instance Functor ApplicationLayerLang where
  fmap f (RunRepo scr g)            = RunRepo scr (f . g)
  fmap f (RunAggregate scr g)       = RunAggregate scr (f . g)
  fmap f (PersistDomainEvent evt a) = PersistDomainEvent evt (f a)
  fmap f (Logging lvl txt a)        = Logging lvl txt (f a)
  fmap f (NextUUID g)               = NextUUID (f . g)

type ApplicationLayer = F ApplicationLayerLang

runRepo :: Repository a -> ApplicationLayer a
runRepo repo = liftF (RunRepo repo id)

nextUUID :: ApplicationLayer UUID
nextUUID = liftF (NextUUID id)

persistDomainEvent :: DomainEvent -> ApplicationLayer ()
persistDomainEvent evt = liftF (PersistDomainEvent evt ())

logging :: LogLevel -> T.Text -> ApplicationLayer ()
logging lvl txt = liftF (Logging lvl txt ())

logDebug :: T.Text -> ApplicationLayer ()
logDebug = logging Debug

logInfo :: T.Text -> ApplicationLayer ()
logInfo = logging Info

logWarn:: T.Text -> ApplicationLayer ()
logWarn = logging Warn

logError :: T.Text -> ApplicationLayer ()
logError = logging Error

accountRepo :: AccountRepoProgram a -> Repository a
accountRepo = AccountRepo

customerRepo :: CustomerRepoProgram a -> Repository a
customerRepo = CustomerRepo

runAggregate :: Aggregate a -> ApplicationLayer a
runAggregate repo = liftF (RunAggregate repo id)

customerAggregate :: CustomerProgram a -> Aggregate a
customerAggregate = CustomerAggregate

accountAggregate :: AccountProgram a -> Aggregate a
accountAggregate = AccountAggregate

runApplication :: ApplicationLayer a -> AppCache -> SqlBackend -> IO a
runApplication prog cache conn = foldF interpret prog
  where
    interpret :: ApplicationLayerLang a -> IO a
    interpret (RunRepo r f)  = do
      f <$> interpretRepo r conn cache
    interpret (RunAggregate a f) = do
      f <$> interpretAggregate a conn cache
    interpret (Logging lvl txt a) = do
      putStrLn $ "LOG " ++ show lvl ++ ": " ++ show txt
      return a
    interpret (NextUUID f) = do
      f <$> nextRandom
    interpret (PersistDomainEvent evt a) = do
      now <- getCurrentTime
      let (evtName, payload) = toPayload evt
      let pe = PersistedEventEntity now evtName False False "" payload
      _ <- DB.insertEvent pe conn
      return a

interpretRepo :: Repository a -> SqlBackend -> AppCache -> IO a
interpretRepo (AccountRepo r)  = runAccountRepo r
interpretRepo (CustomerRepo r) = runCustomerRepo r

interpretAggregate :: Aggregate a -> SqlBackend -> AppCache -> IO a
interpretAggregate (CustomerAggregate a) _  _ = runCustomerAggregate a -- NOTE: customer aggregate does not access cache / db
interpretAggregate (AccountAggregate a) conn cache = runAccountAggregate a conn cache
