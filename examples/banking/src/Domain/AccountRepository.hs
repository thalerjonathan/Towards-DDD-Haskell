module Domain.AccountRepository where

import Control.Monad.Free

import Domain.Customer (CustomerId)
import Domain.Account

data AccountRepoLang a
  = AddAccount Account a 
  | FindAccountsForOwner CustomerId ([Account] -> a)
  | FindAccountByIban Iban (Maybe Account -> a)
  deriving Functor 

type AccountRepoProgram = Free AccountRepoLang

addAccount :: Account -> AccountRepoProgram ()
addAccount a = liftF (AddAccount a ())

findAccountsForOwner:: CustomerId -> AccountRepoProgram [Account]
findAccountsForOwner owner = liftF (FindAccountsForOwner owner id)

findAccountByIban :: Iban -> AccountRepoProgram (Maybe Account)
findAccountByIban iban = liftF (FindAccountByIban iban id)

interpretAccountRepo :: AccountRepoProgram a -> IO a
interpretAccountRepo (Pure a) = return a
interpretAccountRepo (Free (AddAccount _a cont)) = do
  -- TODO: implement
  interpretAccountRepo cont
interpretAccountRepo (Free (FindAccountsForOwner _owner contF)) = do
  -- TODO: implement
  interpretAccountRepo (contF [])
interpretAccountRepo (Free (FindAccountByIban _iban contF)) = do
  -- TODO: implement
  interpretAccountRepo (contF Nothing)