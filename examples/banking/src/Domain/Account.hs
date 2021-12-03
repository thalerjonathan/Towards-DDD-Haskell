{-# LANGUAGE Arrows #-}
module Domain.Account where

import           Control.Monad.Free
import           Data.MonadicStreamFunction
import           Data.MonadicStreamFunction.InternalCore
import           Data.Text
import           Data.Time.Clock
import           Domain.Customer                         (CustomerId, customerIdFromTextUnsafe)
import           Infrastructure.DB.Banking               as DB

newtype Iban = Iban Text deriving Show
type Money = Double
data TXLine = TXLine Money Iban Text Text UTCTime deriving Show

data AccountType = Giro | Savings deriving Show

data AccountCommand
  = Deposit Money
  | Withdraw Money
  | TransferTo Iban Money Text Text
  | ReceiveFrom Iban Money Text Text
  | GetTXLines
  | GetBalance
  | GetOwner
  | GetIban
  | GetType
  deriving Show

data AccountCommandResult
  = AccountException Text
  | ReturnTXLines [TXLine]
  | ReturnBalance Money
  | ReturnOwner CustomerId
  | ReturnIban Iban
  | ReturnType AccountType
  deriving Show

data AccountDomainEvent
  = TransferSent Money Text CustomerId CustomerId Iban Iban
  | TransferFailed Text Money Text CustomerId CustomerId Iban Iban
  deriving Show

data AccountLang a
  = ReadTXLines DB.AccountEntityId ([TXLine] -> a)
  | NewTXLine Money Iban Text Text a
  | EmitEvent AccountDomainEvent a
  deriving Functor

type AccountProgram = Free AccountLang

data AccountState
  = AccountState CustomerId Iban [TXLine] deriving Show

type Account = MSF AccountProgram AccountCommand (Maybe AccountCommandResult)

account :: (Entity AccountEntity) -> Account
account (Entity aid a) = feedback s0 (proc (cmd, s) -> do
    ret <- arrM (handleCommand aid owner iban b aType) -< cmd
    returnA -< (ret, s))
  where
    owner = customerIdFromTextUnsafe $ DB.accountEntityOwner a 
    iban  = Iban $ DB.accountEntityIban a
    b     = (DB.accountEntityBalance a)
    aType = (case DB.accountEntityType a of 
              DB.Giro -> Domain.Account.Giro
              DB.Savings -> Domain.Account.Savings)

    -- TODO: the MSF is useless atm
    s0 = AccountState owner iban []

txLines :: DB.AccountEntityId -> AccountProgram [TXLine]
txLines aid = liftF (ReadTXLines aid id)

newTxLine :: Money -> Iban -> Text -> Text -> AccountProgram ()
newTxLine m i name ref = liftF (NewTXLine m i name ref ())

emitEvent :: AccountDomainEvent -> AccountProgram ()
emitEvent evt = liftF (EmitEvent evt ())

balance :: [TXLine] -> Money
balance = Prelude.foldr (\(TXLine m _ _ _ _) acc -> acc + m) 0

overdraftLimit :: Double
overdraftLimit = -1000

getIban :: Account -> AccountProgram Iban
getIban a = do
  (ret, _) <- unMSF a GetIban
  case ret of
    (Just (ReturnIban i)) -> return i
    _                     -> error "unexpected return in account GetIban"

getBalance :: Account -> AccountProgram Double
getBalance a = do
  (ret, _) <- unMSF a GetBalance
  case ret of
    (Just (ReturnBalance b)) -> return b
    _                        -> error "unexpected return in account GetBalance"

getType :: Account -> AccountProgram AccountType
getType a = do
  (ret, _) <- unMSF a GetType
  case ret of
    (Just (ReturnType i)) -> return i
    _                     -> error "unexpected return in account GetType"

getTXLines :: Account -> AccountProgram [TXLine]
getTXLines a = do
  (ret, _) <- unMSF a GetTXLines
  case ret of
    (Just (ReturnTXLines ts)) -> return ts
    _                         -> error "unexpected return in account GetTXLines"

handleCommand :: DB.AccountEntityId
              -> CustomerId
              -> Iban
              -> Double
              -> AccountType
              -> AccountCommand
              -> AccountProgram (Maybe AccountCommandResult)
handleCommand _ _ iban b _ (Withdraw amount)  = do
  -- bal <- balance <$> txLines
  -- let bal = balance txs
  if b - amount < overdraftLimit
    then do
      let ret = AccountException "Cannot overdraw Giro account by more than -1000!"
      return $ Just ret
    else do
      newTxLine (-amount) iban "Withdraw" "Withdraw"
      return Nothing
handleCommand _ _ iban _ _ (Deposit amount) = do
  newTxLine amount iban "Deposit" "Deposit"
  return Nothing
handleCommand _ owner iban _ _ (TransferTo toIban amount name ref) = do
  -- TODO: wrong data so far, use correct
  emitEvent $ TransferSent amount ref owner owner iban toIban
  newTxLine (-amount) toIban name ref
  return Nothing
handleCommand _ owner iban _ _ (ReceiveFrom fromIban amount name ref) = do
  -- TODO: wrong data so far, use correct
  emitEvent $ TransferFailed "TestError" amount ref owner owner iban fromIban
  newTxLine amount fromIban name ref
  return Nothing
handleCommand _ _ _ b _ GetBalance = do
  -- Just . ReturnBalance . balance <$> txLines
  return $ Just $ ReturnBalance b
handleCommand _ owner _ _ _ GetOwner = do
  return $ Just (ReturnOwner owner)
handleCommand _ _ iban _ _ GetIban = do
  return $ Just (ReturnIban iban)
handleCommand _ _ _ _ accType GetType = do
  return $ Just $ ReturnType accType
handleCommand aid _ _ _ _ GetTXLines = do
  -- return $ Just $ ReturnTXLines txs
  txs <- txLines aid
  return $ Just $ ReturnTXLines $ txs


execCommand :: Account -> AccountCommand -> IO (Account, (Maybe AccountCommandResult, [AccountDomainEvent]))
execCommand a cmd = do
  ((ret, a'), es) <- runAccountAggregate (unMSF a cmd)
  return (a', (ret, es))

execCommands :: Account -> [AccountCommand] -> IO ([Maybe AccountCommandResult], [AccountDomainEvent])
execCommands a cmds = do
  (ret, es) <- runAccountAggregate (embed a cmds)
  return (ret, es)

runAccountAggregate :: AccountProgram a -> IO (a, [AccountDomainEvent])
runAccountAggregate prog = interpret prog []
  where
    interpret :: AccountProgram a -> [AccountDomainEvent] -> IO (a, [AccountDomainEvent])
    interpret (Pure a) acc = return (a, Prelude.reverse acc)
    interpret (Free (ReadTXLines _aid contF)) acc = do
      putStrLn "ReadTXLines"
      interpret (contF []) acc
    interpret (Free (NewTXLine m i name ref cont)) acc = do
      putStrLn "NewTXLine"
      t <- getCurrentTime
      let _tx = TXLine m i name ref t
      interpret cont acc
    interpret (Free (EmitEvent e cont)) acc = do
      putStrLn $ "EmitEvent" ++ show e
      interpret cont (e:acc)
