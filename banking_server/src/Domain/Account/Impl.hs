{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module Domain.Account.Impl
  ( account
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.MonadicStreamFunction
import qualified Data.Text                  as T
import           Domain.Account.Api
import           Domain.Types
import qualified Infrastructure.DB.Banking  as DB

data AccountRead = AccountRead
  { accountDBId  :: DB.AccountEntityId
  , accountOwner :: CustomerId
  , accountIban  :: Iban
  } deriving Show

data AccountState = AccountState
  { accountTXLines :: Maybe  [TXLine]
  , accountBalance :: Money
  } deriving Show

type AccountInternalEffects = ReaderT AccountRead (StateT AccountState AccountEffects)

account :: DB.Entity DB.AccountEntity -> Account
account (DB.Entity aid a) = feedback s0 (proc (cmd, s) -> do
      (mCmdRes, s') <- arrM (uncurry foobar) -< (cmd, s)
      returnA -< (mCmdRes, s'))

  where
    foobar cmd s = do
      let stateRet = runReaderT ((behaviour atype) cmd) r
          ret      = runStateT stateRet s
      ret

    behaviour DB.Giro    = giro
    behaviour DB.Savings = savings

    o     = customerIdFromTextUnsafe $ DB.accountEntityOwner a
    i     = Iban $ DB.accountEntityIban a
    bal   = DB.accountEntityBalance a
    atype = DB.accountEntityType a

    s0    = AccountState Nothing bal
    r     = AccountRead aid o i


overdraftLimit :: Double
overdraftLimit = -1000

balance :: MonadState AccountState m => m Money
balance = gets accountBalance

setBalance :: MonadState AccountState m => Money -> m ()
setBalance m = modify' (\s -> s { accountBalance = m })

addTXLine :: MonadState AccountState m => TXLine -> m ()
addTXLine tx
  = modify' (\s -> case accountTXLines s of
                    Nothing    -> s { accountTXLines = Just [tx]}
                    (Just txs) -> s { accountTXLines = Just (tx : txs)})


owner ::  MonadReader AccountRead m => m CustomerId
owner = asks accountOwner

iban :: MonadReader AccountRead m => m Iban
iban = asks accountIban

txLines :: AccountInternalEffects [TXLine]
txLines = do
  m <- lift $ gets accountTXLines
  case m of
    Nothing -> do
      aid <- asks accountDBId
      txs <- lift $ lift $ fetchTXLines aid
      lift $ modify' (\s -> s { accountTXLines = Just txs })
      return txs
    (Just txs) ->
      return txs

changeBalance :: Money -> AccountInternalEffects ()
changeBalance bal = do
  aid <- asks accountDBId
  setBalance bal
  lift $ lift $ updateBalance aid bal

newTxLine :: Money -> Iban -> T.Text -> T.Text -> AccountInternalEffects TXLine
newTxLine m i name ref = do
  aid <- asks accountDBId
  tx  <- lift $ lift $ persistTXLine aid m i name ref
  addTXLine tx
  return tx

giro :: AccountCommand -> AccountInternalEffects AccountCommandResult
giro (Withdraw amount) = do
  b <- balance
  i <- iban

  let newBalance = b - amount
  if newBalance < overdraftLimit
    then return $ WithdrawResult $ Left "Cannot overdraw Giro account by more than -1000!"
    else do
      changeBalance newBalance
      tx <- newTxLine (-amount) i "Withdraw" "Withdraw"
      return $ WithdrawResult $ Right tx

giro (Deposit amount) = do
  b <- balance
  i <- iban

  let newBalance = b + amount
  tx <- newTxLine amount i "Deposit" "Deposit"
  changeBalance newBalance
  return $ DepositResult $ Right tx

giro (TransferTo toIban amount name ref) = do
  b <- balance
  
  let newBalance = b - amount
  if newBalance < overdraftLimit
    then return $ TransferToResult $ Left "Cannot overdraw Giro account by more than -1000!"
    else do
      changeBalance newBalance
      tx <- newTxLine (-amount) toIban name ref
      -- emitEvent $ TransferSent amount ref owner owner iban toIban
      return $ TransferToResult $ Right tx

giro (ReceiveFrom fromIban amount name ref) = do
  b <- balance
  let newBalance = b + amount

  changeBalance newBalance
  tx <- newTxLine amount fromIban name ref
  -- emitEvent $ TransferSent amount ref owner owner iban toIban
  return $ ReceiveFromResult tx

-- TODO: can we combine giro and savings getters somehow?
giro GetBalance =
  ReturnBalance <$> balance
giro GetOwner =
  ReturnOwner <$> owner
giro GetIban =
  ReturnIban <$> iban
giro GetType =
  return $ ReturnType Giro
giro GetTXLines =
  ReturnTXLines <$> txLines


savings :: AccountCommand -> AccountInternalEffects AccountCommandResult
savings (Withdraw _) = do
  return $ WithdrawResult $ Left "Cannot withdraw money directly from Savings Account! Use transfer of money into a Giro Account of the same customer."

savings (Deposit _) = do
  return $ DepositResult $ Left "Cannot deposit money directly into Savings Account! Use transfer of money from a Giro Account of the same customer."

savings (TransferTo toIban amount name ref) = do
  b <- balance
  
  let newBalance = b - amount
  if newBalance < 0
    then return $ TransferToResult $ Left "Cannot overdraw Savings account!"
    else do
      changeBalance newBalance
      tx <- newTxLine (-amount) toIban name ref
      -- emitEvent $ TransferSent amount ref owner owner iban toIban
      return $ TransferToResult $ Right tx

savings (ReceiveFrom fromIban amount name ref) = do
  b <- balance
  let newBalance = b + amount

  changeBalance newBalance
  tx <- newTxLine amount fromIban name ref
  -- emitEvent $ TransferSent amount ref owner owner iban toIban
  return $ ReceiveFromResult tx

savings GetBalance =
  ReturnBalance <$> balance
savings GetOwner =
  ReturnOwner <$> owner
savings GetIban =
  ReturnIban <$> iban
savings GetType =
  return $ ReturnType Savings
savings GetTXLines =
  ReturnTXLines <$> txLines
