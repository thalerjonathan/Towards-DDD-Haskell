module Domain.FreeMSF.Customer.Repository where

import           Control.Monad.Free.Church
import           Control.Monad.Writer.Lazy
import qualified Data.Text                     as T
import           Database.Persist.Sql
import           Domain.FreeMSF.Customer.Customer
import           Domain.Types
import           Infrastructure.Cache.AppCache
import           Infrastructure.DB.Banking     as DB

data CustomerRepoLang a
  = AddCustomer CustomerId T.Text (Customer -> a)
  | AllCustomers ([Customer] -> a)
  | FindCustomerById CustomerId (Maybe Customer -> a)
  deriving Functor

type CustomerRepoProgram = F CustomerRepoLang

addCustomer :: CustomerId -> T.Text -> CustomerRepoProgram Customer
addCustomer cid cname = liftF (AddCustomer cid cname id)

allCustomers :: CustomerRepoProgram [Customer]
allCustomers = liftF (AllCustomers id)

findCustomerById :: CustomerId -> CustomerRepoProgram (Maybe Customer)
findCustomerById cid = liftF (FindCustomerById cid id)

runCustomerRepo :: CustomerRepoProgram a -> SqlBackend -> AppCache ->  WriterT [IO ()] IO a
runCustomerRepo prog conn cache = foldF interpretCustomerRepo prog
  where
    interpretCustomerRepo :: CustomerRepoLang a ->  WriterT [IO ()] IO a
    interpretCustomerRepo (AddCustomer cid cname f) = do
      let cEntity = CustomerEntity (customerIdToText cid) cname
      _cDbId <- liftIO $ DB.insertCustomer cEntity conn

      -- Customers have changed => simplest solution is to evict their cache region AFTER DB TX has commited
      tell [invalidateCacheRegion cache CustomerCache]

      let c = customer cid cname
      return $ f c

    interpretCustomerRepo (AllCustomers f) = do
      let act = DB.allCustomers conn
      -- cs <- DB.allCustomers conn
      cs <- liftIO $ performCachedAction cache CustomerCache "all" act
      return $ f $ map (\(Entity _ c) ->
        customer (customerIdFromTextUnsafe $ DB.customerEntityDomainId c) (DB.customerEntityName c)) cs

    interpretCustomerRepo (FindCustomerById cid f) = do
      let act = DB.customerByDomainId (customerIdToText cid) conn
      -- m <- DB.customerByDomainId (customerIdToText cid) conn
      m <- liftIO $ performCachedAction cache CustomerCache ("customer_" ++ show cid) act
      case m of
        Nothing -> return $ f Nothing
        (Just (Entity _ c)) -> do
          return $ f $ Just $ customer cid (DB.customerEntityName c)
