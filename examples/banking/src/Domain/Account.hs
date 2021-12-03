{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module Domain.Account where

import           Control.Monad.Free
import           Data.MonadicStreamFunction
import           Data.MonadicStreamFunction.InternalCore
import qualified Data.Text                               as T
import           Data.Time.Clock
import           Database.Persist.Sql
import           Domain.Customer                         (CustomerId,
                                                          customerIdFromTextUnsafe)
import qualified Infrastructure.DB.Banking               as DB
import Control.Monad.Writer.Lazy

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
  = ReadTXLines DB.AccountEntityId ([TXLine] -> a)
  | NewTXLine Money Iban T.Text T.Text a
  | EmitEvent AccountDomainEvent a
  deriving Functor

type AccountProgram = Free AccountLang

data AccountState
  = AccountState CustomerId Iban [TXLine] deriving Show

type AccountEffects = (WriterT [AccountDomainEvent] AccountProgram)

type Account = MSF 
                AccountEffects
                AccountCommand 
                (Maybe AccountCommandResult)

-- TODO: the MSF is useless atm, make it so that its possible to switch into a new behavior

-- TODO: switch into different behaviours if Giro or Savings!
account :: (DB.Entity DB.AccountEntity) -> Account
account (DB.Entity aid a) = feedback s0 (proc (cmd, s) -> do
    ret <- arrM (handleCommand aid owner iban b aType) -< cmd
    returnA -< (ret, s))
  where
    owner = customerIdFromTextUnsafe $ DB.accountEntityOwner a
    iban  = Iban $ DB.accountEntityIban a
    b     = (DB.accountEntityBalance a)
    aType = (case DB.accountEntityType a of
              DB.Giro    -> Domain.Account.Giro
              DB.Savings -> Domain.Account.Savings)
    s0 = AccountState owner iban []

txLines :: DB.AccountEntityId -> AccountProgram [TXLine]
txLines aid = liftF (ReadTXLines aid id)

newTxLine :: Money -> Iban -> T.Text -> T.Text -> AccountProgram ()
newTxLine m i name ref = liftF (NewTXLine m i name ref ())

-- emitEvent :: AccountDomainEvent -> AccountProgram ()
-- emitEvent evt = liftF (EmitEvent evt ())

balance :: [TXLine] -> Money
balance = Prelude.foldr (\(TXLine m _ _ _ _) acc -> acc + m) 0

overdraftLimit :: Double
overdraftLimit = -1000

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
    f _ = error "unexpected return in account GetIban"

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

handleCommand :: DB.AccountEntityId
              -> CustomerId
              -> Iban
              -> Double
              -> AccountType
              -> AccountCommand
              -> AccountEffects (Maybe AccountCommandResult)
handleCommand _ _ iban b _ (Withdraw amount)  = do
  if b - amount < overdraftLimit
    then return $ Just $ WithdrawResult $ Left "Cannot overdraw Giro account by more than -1000!"
    else do
      lift $ newTxLine (-amount) iban "Withdraw" "Withdraw"
      return Nothing

handleCommand _ _ iban _ _ (Deposit amount) = do
  lift $ newTxLine amount iban "Deposit" "Deposit"
  return Nothing

handleCommand _ owner iban _ _ (TransferTo toIban amount name ref) = do
  -- TODO: wrong data so far, use correct
  emitEvent $ TransferSent amount ref owner owner iban toIban
  lift $ newTxLine (-amount) toIban name ref
  return Nothing
  
handleCommand _ owner iban _ _ (ReceiveFrom fromIban amount name ref) = do
  -- TODO: wrong data so far, use correct
  emitEvent $ TransferFailed "TestError" amount ref owner owner iban fromIban
  lift $ newTxLine amount fromIban name ref
  return Nothing

handleCommand _ _ _ b _ GetBalance = do
  return $ Just $ ReturnBalance b
handleCommand _ owner _ _ _ GetOwner = do
  return $ Just (ReturnOwner owner)
handleCommand _ _ iban _ _ GetIban = do
  return $ Just (ReturnIban iban)
handleCommand _ _ _ _ accType GetType = do
  return $ Just $ ReturnType accType
handleCommand aid _ _ _ _ GetTXLines = do
  txs <- lift $ txLines aid
  return $ Just $ ReturnTXLines $ txs

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

runAccountAggregate :: AccountProgram a -> SqlBackend -> IO (a, [AccountDomainEvent])
runAccountAggregate prog conn = interpret prog []
  where
    interpret :: AccountProgram a -> [AccountDomainEvent] -> IO (a, [AccountDomainEvent])
    interpret (Pure a) acc = return (a, Prelude.reverse acc)

    interpret (Free (ReadTXLines aid cont)) acc = do
      txs <- DB.txLinesOfAccount aid conn

      let txVos = map (\(DB.Entity _ tx) ->
                          TXLine
                            (DB.txLineEntityAmount tx)
                            (Iban $ DB.txLineEntityIban tx)
                            (DB.txLineEntityName tx)
                            (DB.txLineEntityReference tx)
                            (DB.txLineEntityTime tx)
                        ) txs

      interpret (cont txVos) acc

    interpret (Free (NewTXLine m i name ref cont)) acc = do
      putStrLn "NewTXLine"
      t <- getCurrentTime
      let _tx = TXLine m i name ref t
      interpret cont acc

    interpret (Free (EmitEvent e cont)) acc = do
      putStrLn $ "EmitEvent" ++ show e
      interpret cont (e:acc)

