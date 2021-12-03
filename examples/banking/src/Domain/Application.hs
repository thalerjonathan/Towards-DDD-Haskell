{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
module Domain.Application where

import           Control.Monad.Free.Church

import           Database.Persist.Sql
import           Domain.AccountLang
import           Domain.AccountRepository
import           Domain.Customer
import           Domain.CustomerRepository

data Repository a
  = AccountRepo (AccountRepoProgram a)
  | CustomerRepo (CustomerRepoProgram a)

data Aggregate a
  = CustomerAggregate (CustomerProgram a)
  | AccountAggregate (AccountProgram a)

data ApplicationLang a
  = forall b. RunRepo (Repository b) (b -> a)
  | forall b. RunAggregate (Aggregate b) (b -> a)

instance Functor ApplicationLang where
  fmap f (RunRepo scr g)      = RunRepo scr (f . g)
  fmap f (RunAggregate scr g) = RunAggregate scr (f . g)

type Application a = F ApplicationLang a

runRepo :: Repository a -> Application a
runRepo repo = liftF (RunRepo repo id)

accountRepo :: AccountRepoProgram a -> Repository a
accountRepo = AccountRepo

customerRepo :: CustomerRepoProgram a -> Repository a
customerRepo = CustomerRepo

runAggregate :: Aggregate a -> Application a
runAggregate repo = liftF (RunAggregate repo id)

customerAggregate :: CustomerProgram a -> Aggregate a
customerAggregate = CustomerAggregate

accountAggregate :: AccountProgram a -> Aggregate a
accountAggregate = AccountAggregate

runApplication :: Application a -> SqlBackend -> IO a
runApplication prog conn = foldF interpret prog
  where
    interpret :: ApplicationLang a -> IO a
    interpret (RunRepo r f)  = do
      f <$> interpretRepo r conn
    interpret (RunAggregate a f) = do
      f <$> interpretAggregate a conn

interpretRepo :: Repository a -> SqlBackend -> IO a
interpretRepo (AccountRepo r)  = runAccountRepo r
interpretRepo (CustomerRepo r) = runCustomerRepo r

interpretAggregate :: Aggregate a -> SqlBackend -> IO a
interpretAggregate (CustomerAggregate a) _ = runCustomerAggregate a
interpretAggregate (AccountAggregate a) conn = runAccountAggregate a conn
