module Domain.AccountRepository where

import           Control.Monad.Free.Church

import           Database.Persist.Sql
import           Domain.Account
import           Domain.Customer           (CustomerId)

--import qualified Infrastructure.DB.Banking as DB

data AccountRepoLang a
  = AddAccount Account a
  | FindAccountsForOwner CustomerId ([Account] -> a)
  | FindAccountByIban Iban (Maybe Account -> a)
  deriving Functor

type AccountRepoProgram = F AccountRepoLang

addAccount :: Account -> AccountRepoProgram ()
addAccount a = liftF (AddAccount a ())

findAccountsForOwner:: CustomerId -> AccountRepoProgram [Account]
findAccountsForOwner owner = liftF (FindAccountsForOwner owner id)

findAccountByIban :: Iban -> AccountRepoProgram (Maybe Account)
findAccountByIban iban = liftF (FindAccountByIban iban id)

runAccountRepo :: AccountRepoProgram a -> SqlBackend -> IO a
runAccountRepo prog _ = foldF interpretAccountRepo prog
  where
    interpretAccountRepo :: AccountRepoLang a -> IO a
    interpretAccountRepo (AddAccount _a ret) = do
      -- TODO: need owner CustomerEntityId
      -- TODO: need a connection
      -- let owner   = CustomerEntityId 0
      --     balance = 0
      --     iban    = ""
      --     aType   = Giro
      -- let accountEntity = AccountEntity owner balance iban aType

      -- DB.insertAccount

      -- TODO: implement
      return ret
    interpretAccountRepo (FindAccountsForOwner _owner cont) = do
      -- TODO: implement
      return (cont [])
    interpretAccountRepo (FindAccountByIban _iban cont) = do
      -- TODO: implement
      return (cont Nothing)
