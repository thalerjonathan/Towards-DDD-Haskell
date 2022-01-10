{-# LANGUAGE RankNTypes #-}
module Test.Application.FreeMSF.Runner where

import           Application.FreeMSF.Layer
import           Control.Monad.Free.Church
import           Control.Monad.Identity
import           Data.Maybe
import           Data.UUID
import           Domain.FreeMSF.Account.Api
import           Domain.FreeMSF.Account.Repository
import           Domain.FreeMSF.Customer.Customer
import           Domain.FreeMSF.Customer.Repository
import           Domain.Types
import           Test.Domain.FreeMSF.Account.Data

customerRepoStub :: CustomerRepoLang a -> Identity a
customerRepoStub (AddCustomer cid cname f) = do
  let c = customer cid cname
  return $ f c
customerRepoStub (AllCustomers cont) = do
  return $ cont []
customerRepoStub (FindCustomerById _ cont) = do
  return $ cont Nothing

accountRepoStub :: AccountRepoLang a -> Identity a
accountRepoStub (AddAccount (CustomerId _cid) _amount _iban aType cont) = do
  let a = mkTestAccount aType
  return $ cont a
accountRepoStub (FindAccountsForOwner (CustomerId _cDomainId) cont) = do
  return $ cont []
accountRepoStub (FindAccountByIban (Iban _i) cont) = do
  return $ cont Nothing

testApplication :: ApplicationLayer a
                -> (forall b. AccountRepoLang b -> Identity b)
                -> (forall b. CustomerRepoLang b -> Identity b)
                -> (forall b. AccountLang b -> Identity b)
                -> a
testApplication prog accRepoInter custRepoInter accAggInter
    = runIdentity $ foldF interpret prog
  where
    interpret :: ApplicationLayerLang a -> Identity a
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
    interpret (RunAggregate (AccountAggregate a) f) = do
      f <$> foldF accAggInter a
    interpret (RunAggregate (CustomerAggregate a) f) = do
      let ret = runIdentity a
      return $ f ret
