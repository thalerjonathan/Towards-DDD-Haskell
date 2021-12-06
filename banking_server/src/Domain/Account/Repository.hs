module Domain.Account.Repository where

import           Control.Monad.Free.Church
import           Data.UUID
import           Database.Persist.Sql
import           Domain.Account.Api
import           Domain.Account.Impl
import           Domain.Types
import qualified Infrastructure.DB.Banking as DB

data AccountRepoLang a
  = AddAccount CustomerId Money Iban AccountType (Account -> a)
  | FindAccountsForOwner CustomerId ([Account] -> a)
  | FindAccountByIban Iban (Maybe Account -> a)
  deriving Functor

type AccountRepoProgram = F AccountRepoLang

addAccount :: CustomerId -> Money -> Iban -> AccountType -> AccountRepoProgram Account
addAccount owner balance iban aType = liftF (AddAccount owner balance iban aType id)

findAccountsForOwner:: CustomerId -> AccountRepoProgram [Account]
findAccountsForOwner owner = liftF (FindAccountsForOwner owner id)

findAccountByIban :: Iban -> AccountRepoProgram (Maybe Account)
findAccountByIban iban = liftF (FindAccountByIban iban id)

runAccountRepo :: AccountRepoProgram a -> SqlBackend -> IO a
runAccountRepo prog conn = foldF interpret prog
  where
    interpret :: AccountRepoLang a -> IO a
    interpret (AddAccount owner balance (Iban iban) aType f) = do
      let aTypeDb = case aType of
                      Giro    -> DB.Giro
                      Savings -> DB.Savings

      let accountEntity = DB.AccountEntity (customerIdToText owner) balance iban aTypeDb
      aid <- DB.insertAccount accountEntity conn

      return $ f $ account (Entity aid accountEntity)
    interpret (FindAccountsForOwner (CustomerId cDomainId) cont) = do
      as <- DB.accountsOfCustomer (toText cDomainId) conn
      let aggs = map (\e -> account e) as
      return $ cont aggs

    interpret (FindAccountByIban (Iban i) cont) = do
      m <- DB.accountByIban i conn
      case m of
        Nothing  -> return (cont Nothing)
        (Just e) -> return (cont $ Just $ account e)
