module Domain.Eff.Account where

import           Application.Eff.Layer
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Text                     as T
import           Data.Time.Clock
import           Domain.Types
import           Infrastructure.Cache.AppCache
import qualified Infrastructure.DB.Banking     as DB

data AccountType = Giro | Savings deriving (Show, Eq, Read)

data Account = Account
  { _aDbId    :: DB.AccountEntityId
  , _aOwner   :: CustomerId
  , _aIban    :: Iban
  , _aBalance :: Double
  , _aType    :: AccountType
  }

data TxLine = TxLine
  { _txIban   :: Iban
  , _txName   :: T.Text
  , _txRef    :: T.Text
  , _txAmount :: Money
  , _txTime   :: UTCTime
  }

overdraftLimit :: Money
overdraftLimit = -1000.0

class Monad m => AccountAggregate m where
  accountTxLines     :: Account -> m [TxLine]
  accountDeposit     :: Account -> Money -> ExceptT T.Text m (TxLine, Account)
  accountWithdraw    :: Account -> Money -> ExceptT T.Text m (TxLine, Account)
  accountTransferTo  :: Account -> Iban -> Money -> T.Text -> T.Text -> ExceptT T.Text m (TxLine, Account)
  accountReceiveFrom :: Account -> Iban -> Money -> T.Text -> T.Text -> ExceptT T.Text m (TxLine, Account)

instance AccountAggregate AppCtx where
  accountTxLines :: Account -> AppCtx [TxLine]
  accountTxLines a = do
    conn  <- asks _conn
    cache <- asks _cache

    let aid = _aDbId a
        act = DB.txLinesOfAccount aid conn

    txs <- liftIO $ performCachedAction cache TxLineCache ("account_" ++ show aid) act

    let txVos = map (\(DB.Entity _ tx) ->
                        TxLine
                          (Iban $ DB.txLineEntityIban tx)
                          (DB.txLineEntityName tx)
                          (DB.txLineEntityReference tx)
                          (DB.txLineEntityAmount tx)
                          (DB.txLineEntityTime tx)
                      ) txs

    return txVos

  accountDeposit :: Account -> Money -> ExceptT T.Text AppCtx (TxLine, Account)
  accountDeposit a@(Account aid _owner (Iban i) _balance _atype) amount = do
    conn  <- asks _conn
    cache <- asks _cache
    now   <- liftIO getCurrentTime

    (newBalance, tx, a') <- performAccountDeposit a amount now

    _txKey <- liftIO $ DB.insertTXLine (DB.TxLineEntity aid i amount (_txName tx) (_txRef tx) now) conn
    liftIO $ DB.updateAccountBalance aid newBalance conn

    -- TXLines have changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache TxLineCache]
    -- Account has changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache AccountCache]

    return (tx, a')

  accountWithdraw :: Account -> Money -> ExceptT T.Text AppCtx (TxLine, Account)
  accountWithdraw a@(Account aid _owner (Iban i) _balance _atype) amount = do
    conn  <- asks _conn
    cache <- asks _cache
    now   <- liftIO getCurrentTime

    (newBalance, tx, a') <- performAccountWithdraw a amount now

    _txKey <- liftIO $ DB.insertTXLine (DB.TxLineEntity aid i amount (_txName tx) (_txRef tx) now) conn
    liftIO $ DB.updateAccountBalance aid newBalance conn

    -- TXLines have changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache TxLineCache]
    -- Account has changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache AccountCache]

    return (tx, a')

  accountTransferTo  :: Account -> Iban -> Money -> T.Text -> T.Text -> ExceptT T.Text AppCtx (TxLine, Account)
  accountTransferTo a@(Account aid _owner _i _balance _atype) toIban@(Iban toIbanStr) amount name ref = do
    conn  <- asks _conn
    cache <- asks _cache
    now   <- liftIO getCurrentTime

    (newBalance, tx, a') <- performAccounTransferTo a toIban amount name ref now

    _txKey <- liftIO $ DB.insertTXLine (DB.TxLineEntity aid toIbanStr amount name ref now) conn
    liftIO $ DB.updateAccountBalance aid newBalance conn

    -- TXLines have changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache TxLineCache]
    -- Account has changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache AccountCache]

    return (tx, a')

  accountReceiveFrom :: Account -> Iban -> Money -> T.Text -> T.Text -> ExceptT T.Text AppCtx (TxLine, Account)
  accountReceiveFrom a@(Account aid _owner _i _balance _atype) fromIban@(Iban fromIbanStr) amount name ref = do
    conn  <- asks _conn
    cache <- asks _cache
    now   <- liftIO getCurrentTime

    (newBalance, tx, a') <- performAccounReceiveFrom a fromIban amount name ref now

    _txKey <- liftIO $ DB.insertTXLine (DB.TxLineEntity aid fromIbanStr amount name ref now) conn
    liftIO $ DB.updateAccountBalance aid newBalance conn

    -- TXLines have changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache TxLineCache]
    -- Account has changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache AccountCache]

    return (tx, a')

performAccountDeposit :: Monad m 
                      => Account 
                      -> Money 
                      -> UTCTime
                      -> ExceptT T.Text m (Money, TxLine, Account)
performAccountDeposit (Account _ _ _ _ Savings) _ _
  = throwError "Cannot deposit money directly into Savings Account! Use transfer of money from a Giro Account of the same customer."
performAccountDeposit a@(Account _ _ i balance Giro) amount now = do
    let newBalance = balance + amount
    return (newBalance, tx, a { _aBalance = newBalance })
  where
    tx = TxLine i "Deposit" "Deposit" amount now

performAccountWithdraw :: Monad m 
                       => Account 
                       -> Money 
                       -> UTCTime
                       -> ExceptT T.Text m (Money, TxLine, Account)
performAccountWithdraw (Account _ _ _ _ Savings) _ _
  = throwError "Cannot withdraw money directly from Savings Account! Use transfer of money into a Giro Account of the same customer."
performAccountWithdraw a@(Account _ _ i balance Giro) amount now = do
    let newBalance = balance - amount
    if newBalance < overdraftLimit
      then throwError "Cannot overdraw Giro account by more than -1000!"
      else return (newBalance, tx, a { _aBalance = newBalance })
  where
    tx = TxLine i "Withdraw" "Withdraw" (-amount) now

performAccounTransferTo :: Monad m 
                        => Account 
                        -> Iban
                        -> Money 
                        -> T.Text
                        -> T.Text
                        -> UTCTime
                        -> ExceptT T.Text m (Money, TxLine, Account)
performAccounTransferTo a@(Account _ _ _ balance Savings) toIban amount name ref now = do
    let newBalance = balance - amount
    if newBalance < 0
      then throwError "Cannot overdraw Savings account!"
      else return (newBalance, tx, a { _aBalance = newBalance })
  where
    tx = TxLine toIban name ref (-amount) now
performAccounTransferTo a@(Account _ _ _ balance Giro) toIban amount name ref now = do
    let newBalance = balance - amount
    if newBalance < overdraftLimit
      then throwError "Cannot overdraw Giro account by more than -1000!" 
      else return (newBalance, tx, a { _aBalance = newBalance })
  where
    tx = TxLine toIban name ref (-amount) now

performAccounReceiveFrom :: Monad m 
                         => Account 
                         -> Iban
                         -> Money 
                         -> T.Text
                         -> T.Text
                         -> UTCTime
                         -> ExceptT T.Text m (Money, TxLine, Account)
performAccounReceiveFrom a@(Account _ _ _ balance _) fromIban amount name ref now = do
    let newBalance = balance + amount
    return (newBalance, tx, a { _aBalance = newBalance })
  where
    tx = TxLine fromIban name ref amount now