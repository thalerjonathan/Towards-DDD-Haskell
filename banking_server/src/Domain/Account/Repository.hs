module Domain.Account.Repository where

import           Control.Monad.Free.Church
import           Data.UUID
import           Database.Persist.Sql
import           Domain.Account.Api
import           Domain.Account.Impl
import           Domain.Types
import           Infrastructure.Cache.AppCache
import qualified Infrastructure.DB.Banking     as DB

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

runAccountRepo :: AccountRepoProgram a -> SqlBackend -> AppCache -> IO a
runAccountRepo prog conn cache = foldF interpret prog
  where
    interpret :: AccountRepoLang a -> IO a
    interpret (AddAccount owner balance (Iban iban) aType f) = do
      let aTypeDb = case aType of
                      Giro    -> DB.Giro
                      Savings -> DB.Savings
      let accountEntity = DB.AccountEntity (customerIdToText owner) balance iban aTypeDb
      aid <- DB.insertAccount accountEntity conn
      -- TODO: easies solution: evict AccountCache region 
      return $ f $ account (Entity aid accountEntity)

    interpret (FindAccountsForOwner (CustomerId cDomainId) cont) = do
      let act = DB.accountsOfCustomer (toText cDomainId) conn
      as <- performCachedAction cache AccountCache ("owner_" ++ show cDomainId) act
      --as <- DB.accountsOfCustomer (toText cDomainId) conn
      return $ cont $ map account as

    interpret (FindAccountByIban (Iban i) cont) = do
      let act = DB.accountByIban i conn
      m <- performCachedAction cache AccountCache ("iban_" ++ show i) act
      -- m <- DB.accountByIban i conn
      case m of
        Nothing  -> return (cont Nothing)
        (Just e) -> return (cont $ Just $ account e)
