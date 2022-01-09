module Domain.Eff.Customer.Repository where

import           Application.Eff.Layer
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Text                     as T
import           Domain.Eff.Customer
import           Domain.Types
import           Infrastructure.Cache.AppCache
import           Infrastructure.DB.Banking     as DB

class Monad m => CustomerRepo m where
  addCustomer      :: CustomerId -> T.Text -> m Customer
  allCustomers     :: m [Customer]
  findCustomerById :: CustomerId -> m (Maybe Customer)

instance CustomerRepo AppCtx where
  addCustomer :: CustomerId -> T.Text -> AppCtx Customer
  addCustomer cid cname = do
    let cEntity = CustomerEntity (customerIdToText cid) cname

    conn   <- asks _conn
    cache  <- asks _cache

    _cDbId <- liftIO $ DB.insertCustomer cEntity conn

    -- Customers have changed => simplest solution is to evict their cache region AFTER DB TX has commited
    tell [invalidateCacheRegion cache CustomerCache]

    let c = Customer cid cname
    return c


  allCustomers :: AppCtx [Customer]
  allCustomers = do
    conn  <- asks _conn
    cache <- asks _cache

    let act = DB.allCustomers conn

    cs <- liftIO $ performCachedAction cache CustomerCache "all" act
    return $ map (\(Entity _ c) -> Customer (customerIdFromTextUnsafe $ DB.customerEntityDomainId c) (DB.customerEntityName c)) cs

  findCustomerById :: CustomerId -> AppCtx (Maybe Customer)
  findCustomerById cid = do
    conn  <- asks _conn
    cache <- asks _cache

    let act = DB.customerByDomainId (customerIdToText cid) conn
    m <- liftIO $ performCachedAction cache CustomerCache ("customer_" ++ show cid) act
    case m of
      Nothing -> return Nothing
      (Just (Entity _ c)) -> do
        return $ Just $ Customer cid (DB.customerEntityName c)
