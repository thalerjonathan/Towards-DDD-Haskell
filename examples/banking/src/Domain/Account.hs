{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module Domain.Account where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.MonadicStreamFunction
import           Domain.AccountLang
import           Domain.Customer            (CustomerId,
                                             customerIdFromTextUnsafe)
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

account :: (DB.Entity DB.AccountEntity) -> Account
account (DB.Entity aid a) = feedback s0 (proc (cmd, s) -> do
      (mCmdRes, s') <- arrM (uncurry foobar) -< (cmd, s)
      returnA -< (mCmdRes, s')
  )

  where
    foobar cmd s = do
      let stateRet = runReaderT (giroTest cmd) r
          ret = runStateT stateRet s
      ret

    --behaviour DB.Giro    = giro 
    --behaviour DB.Savings = savings
    --beh   = behaviour atype
    
    owner = customerIdFromTextUnsafe $ DB.accountEntityOwner a
    iban  = Iban $ DB.accountEntityIban a
    bal   = (DB.accountEntityBalance a)
    _atype = DB.accountEntityType a
    

    s0    = AccountState Nothing bal
    r     = AccountRead aid owner iban

giroTest :: AccountCommand -> AccountInternalEffects (Maybe AccountCommandResult)
giroTest _ = return Nothing

{-
giro :: AccountCommand -> AccountInternalEffects (Maybe AccountCommandResult)
giro aid _ iban bal (Withdraw amount) = do
  let newBalance = bal - amount 
  if newBalance < overdraftLimit
    then return $ Just $ WithdrawResult $ Left "Cannot overdraw Giro account by more than -1000!"
    else do
      -- TODO: update balance in Aggregate state, so it reflects the balance the next time
      -- TODO: add TXLine to Aggregate state, so it is reflected in a subsequent call
      tx <- newTxLine aid (-amount) iban "Withdraw" "Withdraw"
      changeBalance aid newBalance
      return $ Just $ WithdrawResult $ Right tx

giro aid _ iban bal (Deposit amount) = do
  -- TODO: update balance in Aggregate state, so it reflects the balance the next time
  -- TODO: add TXLine to Aggregate state, so it is reflected in a subsequent call
  let newBalance = bal + amount 
  tx <- newTxLine aid amount iban "Deposit" "Deposit"
  changeBalance aid newBalance
  return $ Just $ DepositResult $ Right tx

giro aid owner iban _ (TransferTo toIban amount name ref) = do
  -- TODO: check overdraft limit
  emitEvent $ TransferSent amount ref owner owner iban toIban
  _tx <- newTxLine aid (-amount) toIban name ref
  return Nothing

giro aid owner iban _ (ReceiveFrom fromIban amount name ref) = do
  emitEvent $ TransferFailed "TestError" amount ref owner owner iban fromIban
  _tx <- newTxLine aid amount fromIban name ref
  return Nothing

-- TODO: can we combine giro and savings getters somehow?
giro _ _ _ b GetBalance = do
  return $ Just $ ReturnBalance b
giro _ owner _ _ GetOwner = do
  return $ Just (ReturnOwner owner)
giro _ _ iban _ GetIban = do
  return $ Just (ReturnIban iban)
giro _ _ _ _ GetType = do
  return $ Just $ ReturnType Giro
giro aid _ _ _ GetTXLines = do
  txs <- txLines aid
  return $ Just $ ReturnTXLines $ txs


savings :: AccountCommand -> AccountEffects (Maybe AccountCommandResult)
savings _ _ _ _ (Withdraw _) = do
  return $ Just $ WithdrawResult $ Left "Cannot withdraw money directly from Savings Account! Use transfer of money into a Giro Account of the same customer."
savings _ _ _ _ (Deposit _) = do
  return $ Just $ DepositResult $ Left "Cannot deposit money directly into Savings Account! Use transfer of money from a Giro Account of the same customer."

savings aid owner iban _ (TransferTo toIban amount name ref) = do
  -- TODO: check overdraft limit
  emitEvent $ TransferSent amount ref owner owner iban toIban
  _tx <- newTxLine aid (-amount) toIban name ref
  return Nothing

savings aid owner iban _ (ReceiveFrom fromIban amount name ref) = do
  emitEvent $ TransferFailed "TestError" amount ref owner owner iban fromIban
  _tx <- newTxLine aid amount fromIban name ref
  return Nothing

-- TODO: can we combine giro and savings getters somehow?
savings _ _ _ b GetBalance = do
  return $ Just $ ReturnBalance b
savings _ owner _ _ GetOwner = do
  return $ Just (ReturnOwner owner)
savings _ _ iban _ GetIban = do
  return $ Just (ReturnIban iban)
savings _ _ _ _ GetType = do
  return $ Just $ ReturnType Savings
savings aid _ _ _ GetTXLines = do
  txs <- txLines aid
  return $ Just $ ReturnTXLines $ txs

balance :: [TXLine] -> Money
balance = Prelude.foldr (\(TXLine m _ _ _ _) acc -> acc + m) 0

overdraftLimit :: Double
overdraftLimit = -1000
-}