{-# LANGUAGE FlexibleContexts #-}
module Domain.Account.Api where

import           Control.Monad.Free.Church
import           Control.Monad.Writer.Lazy
import           Data.MonadicStreamFunction
import           Data.MonadicStreamFunction.InternalCore
import qualified Data.Text                               as T
import           Data.Time.Clock
import           Database.Persist.Sql
import           Domain.Types
import           Infrastructure.Cache.AppCache
import qualified Infrastructure.DB.Banking               as DB

data TXLine  = TXLine Money Iban T.Text T.Text UTCTime deriving Show

data AccountType = Giro | Savings deriving (Show, Eq, Read)

data AccountCommand
  = Deposit Money
  | Withdraw Money
  | TransferTo Iban Money T.Text T.Text
  | ReceiveFrom Iban Money T.Text T.Text
  | GetTXLines
  | GetBalance
  | GetOwner
  | GetIban
  | GetType
  deriving Show

data AccountCommandResult
  = AccountException T.Text
  | DepositResult (Either T.Text TXLine)
  | WithdrawResult (Either T.Text TXLine)
  | TransferToResult (Either T.Text TXLine)
  | ReceiveFromResult TXLine
  | ReturnTXLines [TXLine]
  | ReturnBalance Money
  | ReturnOwner CustomerId
  | ReturnIban Iban
  | ReturnType AccountType
  deriving Show

data AccountDomainEvent
  = TransferSent Money T.Text CustomerId CustomerId Iban Iban
  | TransferFailed T.Text Money T.Text CustomerId CustomerId Iban Iban
  deriving Show

data AccountLang a
  = FetchTXLines DB.AccountEntityId ([TXLine] -> a)
  | PersistTXLine DB.AccountEntityId Money Iban T.Text T.Text (TXLine -> a)
  | UpdateBalance DB.AccountEntityId Money a
  deriving Functor

type AccountProgram = F AccountLang

type AccountEffects = (WriterT [AccountDomainEvent] AccountProgram)

type Account = MSF
                AccountEffects
                AccountCommand
                AccountCommandResult

execCommand :: Account -> AccountCommand -> AccountProgram (Account, AccountCommandResult, [AccountDomainEvent])
execCommand a cmd = do
  ((ret, a'), domainEvts) <- runWriterT $ unMSF a cmd
  return (a', ret, domainEvts)

getCommand :: Account -> AccountCommand -> (AccountCommandResult -> a) -> AccountProgram a
getCommand a cmd f = do
  ((ret, _), _) <- runWriterT $ unMSF a cmd
  return (f ret)

deposit :: Account -> Money -> AccountProgram (Account, AccountCommandResult, [AccountDomainEvent])
deposit a amount = execCommand a (Deposit amount)

withdraw :: Account -> Money -> AccountProgram (Account, AccountCommandResult, [AccountDomainEvent])
withdraw a amount = execCommand a (Withdraw amount)

transferTo :: Account -> Iban -> Money -> T.Text -> T.Text -> AccountProgram (Account, AccountCommandResult, [AccountDomainEvent])
transferTo a toIban amount name ref = execCommand a (TransferTo toIban amount name ref)

receiveFrom :: Account -> Iban -> Money -> T.Text -> T.Text -> AccountProgram (Account, AccountCommandResult, [AccountDomainEvent])
receiveFrom a fromIban amount name ref = execCommand a (ReceiveFrom fromIban amount name ref)

getIban' :: Account -> AccountProgram Iban
getIban' a = getCommand a GetIban f
  where
    f (ReturnIban i) = i
    f _              = error "unexpected return in account GetIban"

getOwner :: Account -> AccountProgram CustomerId
getOwner a = do
  ((ret, _), _) <- runWriterT $ unMSF a GetOwner
  case ret of
    (ReturnOwner o) -> return o
    _               -> error "unexpected return in account GetOwner"

getIban :: Account -> AccountProgram Iban
getIban a = do
  ((ret, _), _) <- runWriterT $ unMSF a GetIban
  case ret of
    (ReturnIban i) -> return i
    _              -> error "unexpected return in account GetIban"

getBalance :: Account -> AccountProgram Double
getBalance a = do
  ((ret, _), _) <- runWriterT $ unMSF a GetBalance
  case ret of
    (ReturnBalance b) -> return b
    _                 -> error "unexpected return in account GetBalance"

getType :: Account -> AccountProgram AccountType
getType a = do
  ((ret, _), _) <- runWriterT $ unMSF a GetType
  case ret of
    (ReturnType i) -> return i
    _              -> error "unexpected return in account GetType"

getTXLines :: Account -> AccountProgram [TXLine]
getTXLines a = do
  ((ret, _), _) <- runWriterT $ unMSF a GetTXLines
  case ret of
    (ReturnTXLines ts) -> return ts
    _                  -> error "unexpected return in account GetTXLines"

emitEvent :: Monad m => AccountDomainEvent -> WriterT [AccountDomainEvent] m ()
emitEvent e = tell [e]

fetchTXLines :: DB.AccountEntityId -> AccountEffects [TXLine]
fetchTXLines aid = lift $ liftF (FetchTXLines aid id)

persistTXLine :: DB.AccountEntityId -> Money -> Iban -> T.Text -> T.Text -> AccountEffects TXLine
persistTXLine aid m i name ref = lift $ liftF (PersistTXLine aid m i name ref id)

updateBalance :: DB.AccountEntityId -> Money -> AccountEffects ()
updateBalance aid m = lift $ liftF (UpdateBalance aid m ())

runAccountAggregate :: AccountProgram a -> SqlBackend -> AppCache ->  IO a
runAccountAggregate prog conn cache = foldF interpret prog
  where
    interpret :: AccountLang a -> IO a
    interpret (FetchTXLines aid cont)  = do
      let act = DB.txLinesOfAccount aid conn
      txs <- performCachedAction cache TxLineCache ("account_" ++ show aid) act
      --txs <- DB.txLinesOfAccount aid conn

      let txVos = map (\(DB.Entity _ tx) ->
                          TXLine
                            (DB.txLineEntityAmount tx)
                            (Iban $ DB.txLineEntityIban tx)
                            (DB.txLineEntityName tx)
                            (DB.txLineEntityReference tx)
                            (DB.txLineEntityTime tx)
                        ) txs

      return (cont txVos)

    interpret (PersistTXLine aid m iban@(Iban i) name ref cont) = do
      -- TXLines have changed => simplest solution is to evict their cache region
      evictCacheRegion cache TxLineCache

      now    <- getCurrentTime
      _txKey <- DB.insertTXLine (DB.TxLineEntity aid i m name ref now) conn
      let tx = TXLine m iban name ref now

      return (cont tx)

    interpret (UpdateBalance aid m a) = do 
      -- Account has changed => simplest solution is to evict their cache region
      evictCacheRegion cache AccountCache

      DB.updateAccountBalance aid m conn

      return a
