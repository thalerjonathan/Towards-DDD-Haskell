module Domain.Eff.Account where

import           Application.Eff.Layer
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Text                     as T
import           Data.Time.Clock
import           Database.Persist.Sql
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
  accountDeposit (Account _ _ _ _ Savings) _ 
    = throwError "Cannot deposit money directly into Savings Account! Use transfer of money from a Giro Account of the same customer."
  accountDeposit a@(Account aid _owner i balance Giro) amount = do
    conn  <- asks _conn
    cache <- asks _cache

    let newBalance = balance + amount
        a'         = a { _aBalance = newBalance }

    tx <- newTxLine aid i amount "Deposit" "Deposit" conn
    liftIO $ DB.updateAccountBalance aid newBalance conn

    -- TXLines have changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache TxLineCache]
    -- Account has changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache AccountCache]

    return (tx, a')

  -- TODO: businees checks!
  accountWithdraw :: Account -> Money -> ExceptT T.Text AppCtx (TxLine, Account)
  accountWithdraw (Account _ _ _ _ Savings) _ 
    = throwError "Cannot withdraw money directly from Savings Account! Use transfer of money into a Giro Account of the same customer."
  accountWithdraw a@(Account aid _owner i balance Giro) amount = do
    conn  <- asks _conn
    cache <- asks _cache

    let newBalance = balance - amount
        a'         = a { _aBalance = newBalance }

    tx <- newTxLine aid i amount "Withdraw" "Withdraw" conn
    liftIO $ DB.updateAccountBalance aid newBalance conn

    -- TXLines have changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache TxLineCache]
    -- Account has changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache AccountCache]

    return (tx, a')

  accountTransferTo  :: Account -> Iban -> Money -> T.Text -> T.Text -> ExceptT T.Text m (TxLine, Account)
  accountTransferTo = undefined

  accountReceiveFrom :: Account -> Iban -> Money -> T.Text -> T.Text -> ExceptT T.Text m (TxLine, Account)
  accountReceiveFrom = undefined

newTxLine :: MonadIO m 
          => DB.AccountEntityId
          -> Iban
          -> Double
          -> T.Text
          -> T.Text
          -> SqlBackend
          -> m TxLine
newTxLine aid i@(Iban ibanStr) amount name ref conn = do
  now    <- liftIO getCurrentTime
  _txKey <- liftIO $ DB.insertTXLine (DB.TxLineEntity aid ibanStr amount name ref now) conn
  return $ TxLine i name ref amount now
