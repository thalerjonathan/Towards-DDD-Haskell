{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module Domain.Account.Api where

import           Control.Monad.Free.Church
import           Control.Monad.Writer.Lazy
import           Data.MonadicStreamFunction
import           Data.MonadicStreamFunction.InternalCore
import qualified Data.Text                               as T
import           Data.Time.Clock
import           Database.Persist.Sql
import           Domain.Customer                         (CustomerId)
import qualified Infrastructure.DB.Banking               as DB

newtype Iban = Iban T.Text deriving Show
type Money   = Double
data TXLine  = TXLine Money Iban T.Text T.Text UTCTime deriving Show

data AccountType = Giro | Savings deriving Show

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
                (Maybe AccountCommandResult)

execCommand :: Account -> AccountCommand -> AccountProgram (Account, Maybe AccountCommandResult, [AccountDomainEvent])
execCommand a cmd = do
  ((ret, a'), domainEvts) <- runWriterT $ unMSF a cmd
  return (a', ret, domainEvts)

getCommand :: Account -> AccountCommand -> (Maybe AccountCommandResult -> a) -> AccountProgram a
getCommand a cmd f = do
  ((ret, _), _) <- runWriterT $ unMSF a cmd
  return (f ret)

deposit :: Account -> Money -> AccountProgram (Account, Maybe AccountCommandResult, [AccountDomainEvent])
deposit a amount = execCommand a (Deposit amount)

withdraw :: Account -> Money -> AccountProgram (Account, Maybe AccountCommandResult, [AccountDomainEvent])
withdraw a amount = execCommand a (Withdraw amount)

getIban' :: Account -> AccountProgram Iban
getIban' a = getCommand a GetIban f
  where
    f (Just (ReturnIban i)) = i
    f _                     = error "unexpected return in account GetIban"

getIban :: Account -> AccountProgram Iban
getIban a = do
  ((ret, _), _) <- runWriterT $ unMSF a GetIban
  case ret of
    (Just (ReturnIban i)) -> return i
    _                     -> error "unexpected return in account GetIban"

getBalance :: Account -> AccountProgram Double
getBalance a = do
  ((ret, _), _) <- runWriterT $ unMSF a GetBalance
  case ret of
    (Just (ReturnBalance b)) -> return b
    _                        -> error "unexpected return in account GetBalance"

getType :: Account -> AccountProgram AccountType
getType a = do
  ((ret, _), _) <- runWriterT $ unMSF a GetType
  case ret of
    (Just (ReturnType i)) -> return i
    _                     -> error "unexpected return in account GetType"

getTXLines :: Account -> AccountProgram [TXLine]
getTXLines a = do
  ((ret, _), _) <- runWriterT $ unMSF a GetTXLines
  case ret of
    (Just (ReturnTXLines ts)) -> return ts
    _                         -> error "unexpected return in account GetTXLines"

emitEvent :: Monad m => AccountDomainEvent -> WriterT [AccountDomainEvent] m ()
emitEvent e = tell [e]

fetchTXLines :: DB.AccountEntityId -> AccountEffects [TXLine]
fetchTXLines aid = lift $ liftF (FetchTXLines aid id)

persistTXLine :: DB.AccountEntityId -> Money -> Iban -> T.Text -> T.Text -> AccountEffects TXLine
persistTXLine aid m i name ref = lift $ liftF (PersistTXLine aid m i name ref id)

updateBalance :: DB.AccountEntityId -> Money -> AccountEffects ()
updateBalance aid m = lift $ liftF (UpdateBalance aid m ())

{-
execCommand :: Account -> AccountCommand -> IO (Account, (Maybe AccountCommandResult, [AccountDomainEvent]))
execCommand a cmd = do
  ((ret, a'), es) <- runAccountAggregate (unMSF a cmd)
  return (a', (ret, es))

execCommands :: Account -> [AccountCommand] -> IO ([Maybe AccountCommandResult], [AccountDomainEvent])
execCommands a cmds = do
  (ret, es) <- runAccountAggregate (embed a cmds)
  return (ret, es)
-}

-- TODO: put run into separate interpreter

runAccountAggregate :: AccountProgram a -> SqlBackend -> IO a
runAccountAggregate prog conn = foldF interpret prog
  where
    interpret :: AccountLang a -> IO a
    interpret (FetchTXLines aid cont)  = do
      txs <- DB.txLinesOfAccount aid conn

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
      now    <- getCurrentTime
      _txKey <- DB.insertTXLine (DB.TxLineEntity aid i m name ref now) conn
      let tx = TXLine m iban name ref now

      return (cont tx)

    interpret (UpdateBalance aid m a) = do
      DB.updateAccountBalance aid m conn
      return a