{-# LANGUAGE RankNTypes #-}
module Test.Application.Runner where

import           Control.Monad.Free.Church
import           Control.Monad.Identity
import           Data.Maybe
import           Data.UUID
import           Domain.Account.Repository
import           Domain.Application
import           Domain.Customer.Customer
import           Domain.Customer.Repository
import           Domain.Types

customerRepoStub :: CustomerRepoLang a -> Identity a
customerRepoStub (AddCustomer cid cname f) = do
  let c = customer cid cname
  return $ f c
customerRepoStub (AllCustomers cont) = do
  return $ cont []
customerRepoStub (FindCustomerById _ cont) = do
  return $ cont Nothing

accountRepoStub :: AccountRepoLang a -> Identity a
accountRepoStub (AddAccount _a ret) = do
  return ret
accountRepoStub (FindAccountsForOwner (CustomerId _cDomainId) cont) = do
  return $ cont []
accountRepoStub (FindAccountByIban (Iban _i) cont) = do
  return $ cont Nothing

testApplication :: Application a
                -> (forall b. AccountRepoLang b -> Identity b)
                -> (forall b. CustomerRepoLang b -> Identity b)
                -> a
testApplication prog accRepoInter custRepoInter
    = runIdentity $ foldF interpret prog
  where
    interpret :: ApplicationLang a -> Identity a
    interpret (RunRepo (AccountRepo r) f)  = do
      f <$> foldF accRepoInter r
    interpret (RunRepo (CustomerRepo r) f)  = do
      f <$> foldF custRepoInter r
    interpret (NextUUID f) = do
      let uuid = fromJust $ fromText "176c8054-ec71-4a84-9d5b-abe85a6d955c"
      return $ f uuid
    interpret (Logging _lvl _txt a) = do
      return a
    interpret (PersistDomainEvent _evt _a) = do
      undefined
    interpret (RunAggregate (AccountAggregate _a) _f) = do
      undefined
    interpret (RunAggregate (CustomerAggregate _a) _f) = do
      undefined
