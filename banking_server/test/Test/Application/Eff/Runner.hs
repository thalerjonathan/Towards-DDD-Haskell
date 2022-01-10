{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Application.Eff.Runner where

import           Application.Eff.Layer
import           Application.Events
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                       as Map
import           Data.Text                      as T
import           Data.Time.Clock
import           Data.UUID
import           Domain.Eff.Account
import           Domain.Eff.Account.Repository
import           Domain.Eff.Customer
import           Domain.Eff.Customer.Repository
import           Domain.Types
import           Test.Domain.Utils

data PureAppData = PureAppData
  { _now      :: UTCTime
  , _accounts :: Map.Map Iban Account
  , _custs    :: Map.Map CustomerId Customer
  }

newtype PureAppCtx a = PureAppCtx (Reader PureAppData a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader PureAppData
        )

instance ApplicationLayer PureAppCtx where
  persistDomainEvent :: DomainEvent -> PureAppCtx ()
  persistDomainEvent _evt = return ()

  logging :: LogLevel -> T.Text -> PureAppCtx ()
  logging _lvl _txt = return ()

  nextUUID :: PureAppCtx UUID
  nextUUID = error "nextUUID unimplemented"

instance AccountRepo PureAppCtx where
  addAccount :: CustomerId -> Money -> Iban -> AccountType -> PureAppCtx Account
  addAccount owner balance i at = do
    let aid = mkAccountKey 0
    return $ Account aid owner i balance at

  findAccountsForOwner :: CustomerId -> PureAppCtx [Account]
  findAccountsForOwner _owner = do
    return []

  findAccountByIban :: Iban -> PureAppCtx (Maybe Account)
  findAccountByIban i = do
    as <- asks _accounts
    return $ Map.lookup i as

instance AccountAggregate PureAppCtx where
  accountTxLines :: Account -> PureAppCtx [TxLine]
  accountTxLines _a = do
    return []

  accountDeposit :: Account -> Money -> ExceptT T.Text PureAppCtx (TxLine, Account)
  accountDeposit a@(Account _aid _owner _i _balance _atype) amount = do
    now                   <- asks _now
    (_newBalance, tx, a') <- performAccountDeposit a amount now
    return (tx, a')

  accountWithdraw :: Account -> Money -> ExceptT T.Text PureAppCtx (TxLine, Account)
  accountWithdraw a@(Account _aid _owner _i _balance _atype) amount = do
    now               <- asks _now
    (_newBalance, tx, a') <- performAccountWithdraw a amount now
    return (tx, a')

  accountTransferTo  :: Account -> Iban -> Money -> T.Text -> T.Text -> ExceptT T.Text PureAppCtx (TxLine, Account)
  accountTransferTo a toIban amount name ref = do
    now                   <- asks _now
    (_newBalance, tx, a') <- performAccounTransferTo a toIban amount name ref now
    return (tx, a')

  accountReceiveFrom :: Account -> Iban -> Money -> T.Text -> T.Text -> ExceptT T.Text PureAppCtx (TxLine, Account)
  accountReceiveFrom a fromIban amount name ref = do
    now                   <- asks _now
    (_newBalance, tx, a') <- performAccounReceiveFrom a fromIban amount name ref now
    return (tx, a')

instance CustomerRepo PureAppCtx where
  addCustomer :: CustomerId -> T.Text -> PureAppCtx Customer
  addCustomer cid cname = return $ Customer cid cname

  allCustomers :: PureAppCtx [Customer]
  allCustomers = return []

  findCustomerById :: CustomerId -> PureAppCtx (Maybe Customer)
  findCustomerById cid = do
    cs <- asks _custs
    return $ Map.lookup cid cs


mkPureAppData :: UTCTime
              -> [(Iban, Account)]
              -> [(CustomerId, Customer)]
              -> PureAppData
mkPureAppData now as cs = PureAppData now (Map.fromList as) (Map.fromList cs)

testApplication :: PureAppData -> PureAppCtx a -> a
testApplication pureData (PureAppCtx prog) = runReader prog pureData

