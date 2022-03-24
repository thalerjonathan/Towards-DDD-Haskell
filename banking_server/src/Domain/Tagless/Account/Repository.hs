module Domain.Tagless.Account.Repository where

import           Application.Tagless.Layer
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.UUID
import           Domain.Tagless.Account
import           Domain.Types
import           Infrastructure.Cache.AppCache
import           Infrastructure.DB.Banking     as DB

class Monad m => AccountRepo m where
  addAccount           :: CustomerId -> Money -> Iban -> AccountType -> m Account
  findAccountsForOwner :: CustomerId -> m [Account]
  findAccountByIban    :: Iban -> m (Maybe Account)

instance AccountRepo AppCtx where
  addAccount :: CustomerId -> Money -> Iban -> AccountType -> AppCtx Account
  addAccount owner balance i@(Iban iban) at = do
    conn  <- asks _conn
    cache <- asks _cache

    let accountEntity = DB.AccountEntity (customerIdToText owner) balance iban (accountDomainTypeToDb at)
    aid <- liftIO $ DB.insertAccount accountEntity conn

    -- Accounts have changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache AccountCache]

    return $ Account aid owner i balance at

  findAccountsForOwner :: CustomerId -> AppCtx [Account]
  findAccountsForOwner owner@(CustomerId cDomainId) = do
    conn  <- asks _conn
    cache <- asks _cache

    let act = DB.accountsOfCustomer (toText cDomainId) conn
    as <- liftIO $ performCachedAction cache AccountCache ("owner_" ++ show cDomainId) act
    return $ map (\(Entity aid e) -> Account aid owner (Iban $ DB.accountEntityIban e) (DB.accountEntityBalance e) (accountDbTypeToDomain $ DB.accountEntityType e)) as

  findAccountByIban :: Iban -> AppCtx (Maybe Account)
  findAccountByIban ib@(Iban i) = do
    conn  <- asks _conn
    cache <- asks _cache

    let act = DB.accountByIban i conn
    m <- liftIO $ performCachedAction cache AccountCache ("iban_" ++ show i) act
    case m of
      Nothing  -> return Nothing
      (Just (Entity aid e)) -> return $ Just $ Account aid (customerIdFromTextUnsafe $ DB.accountEntityOwner e) ib (DB.accountEntityBalance e) (accountDbTypeToDomain $ DB.accountEntityType e)

accountDbTypeToDomain :: DB.AccountEntityType -> Domain.Tagless.Account.AccountType
accountDbTypeToDomain DB.Giro    = Domain.Tagless.Account.Giro
accountDbTypeToDomain DB.Savings = Domain.Tagless.Account.Savings

accountDomainTypeToDb :: Domain.Tagless.Account.AccountType -> DB.AccountEntityType
accountDomainTypeToDb Domain.Tagless.Account.Giro    = DB.Giro
accountDomainTypeToDb Domain.Tagless.Account.Savings = DB.Savings
