{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module Domain.Account.Impl
  ( account
  ) where

import qualified Data.Text as T
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.MonadicStreamFunction
import           Domain.Account.Api
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
      let stateRet = runReaderT ((behaviour atype) cmd) r
          ret      = runStateT stateRet s
      ret

    behaviour DB.Giro    = giro 
    behaviour DB.Savings = savings
    
    o     = customerIdFromTextUnsafe $ DB.accountEntityOwner a
    i     = Iban $ DB.accountEntityIban a
    bal   = (DB.accountEntityBalance a)
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
  m <- lift $ gets (accountTXLines)
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

giro :: AccountCommand -> AccountInternalEffects (Maybe AccountCommandResult)
giro (Withdraw amount) = do
  b <- balance
  i <- iban
  
  let newBalance = b - amount 
  if newBalance < overdraftLimit
    then return $ Just $ WithdrawResult $ Left "Cannot overdraw Giro account by more than -1000!"
    else do
      changeBalance newBalance
      tx <- newTxLine (-amount) i "Withdraw" "Withdraw"
      return $ Just $ WithdrawResult $ Right tx

giro (Deposit amount) = do
  b <- balance
  i <- iban

  let newBalance = b + amount 
  tx <- newTxLine amount i "Deposit" "Deposit"
  changeBalance newBalance
  return $ Just $ DepositResult $ Right tx

giro (TransferTo _toIban _amount _name _ref) = do
  -- TODO: implement. check overdraft limit
  -- TODO: update balance
  {-
  fromIban <- owner
  toIban   <- owner
  emitEvent $ TransferSent amount ref owner owner iban toIban
  _tx <- newTxLine (-amount) toIban name ref
  -}
  return Nothing

giro (ReceiveFrom _fromIban _amount _name _ref) = do
  -- TODO: implement
  -- TODO: update balance
  {-
  emitEvent $ TransferFailed "TestError" amount ref owner owner iban fromIban
  _tx <- newTxLine aid amount fromIban name ref
  -}
  return Nothing

-- TODO: can we combine giro and savings getters somehow?
giro GetBalance = 
  (Just . ReturnBalance) <$> balance
giro GetOwner = 
  (Just . ReturnOwner) <$> owner
giro GetIban = 
  (Just . ReturnIban) <$> iban
giro GetType = 
  return $ Just $ ReturnType Giro
giro GetTXLines = do
  txs <- txLines
  return $ Just $ ReturnTXLines $ txs


savings :: AccountCommand -> AccountInternalEffects (Maybe AccountCommandResult)
savings (Withdraw _) = do
  return $ Just $ WithdrawResult $ Left "Cannot withdraw money directly from Savings Account! Use transfer of money into a Giro Account of the same customer."

savings (Deposit _) = do
  return $ Just $ DepositResult $ Left "Cannot deposit money directly into Savings Account! Use transfer of money from a Giro Account of the same customer."

savings (TransferTo _toIban _amount _name _ref) = do
  -- TODO: implement. check overdraft limit
  -- TODO: update balance
  return Nothing

savings (ReceiveFrom _fromIban _amount _name _ref) = do
  -- TODO: implement
  -- TODO: update balance
  return Nothing

-- TODO: can we combine giro and savings getters somehow?
savings GetBalance = 
  (Just . ReturnBalance) <$> balance
savings GetOwner = 
  (Just . ReturnOwner) <$> owner
savings GetIban = 
  (Just . ReturnIban) <$> iban
savings GetType = 
  return $ Just $ ReturnType Savings
savings GetTXLines = do
  txs <- txLines
  return $ Just $ ReturnTXLines $ txs