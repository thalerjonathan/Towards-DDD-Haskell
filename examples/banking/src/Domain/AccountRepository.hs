module Domain.AccountRepository where

import           Control.Monad.Free.Church
import           Data.UUID
import           Database.Persist.Sql
import           Domain.Account
import           Domain.AccountLang
import           Domain.Customer
import           Infrastructure.DB.Banking as DB

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
runAccountRepo prog conn = foldF interpret prog
  where
    interpret :: AccountRepoLang a -> IO a
    interpret (AddAccount _a ret) = do
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
    interpret (FindAccountsForOwner (CustomerId cDomainId) cont) = do
      as <- DB.accountsOfCustomer (toText cDomainId) conn
      let aggs = map (\e -> account e) as
      return $ cont aggs

    interpret (FindAccountByIban (Iban i) cont) = do
      m <- DB.accountByIban i conn
      case m of 
        Nothing  -> return (cont Nothing)
        (Just e) -> return (cont $ Just $ account e)
