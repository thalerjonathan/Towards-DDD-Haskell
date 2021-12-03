module Domain.CustomerRepository where

import           Control.Monad.Free.Church
import           Database.Persist.Sql
import           Domain.Customer
import           Infrastructure.DB.Banking as DB

data CustomerRepoLang a
  = AddCustomer Customer a
  | AllCustomers ([Customer] -> a)
  | FindCustomerById CustomerId (Maybe Customer -> a)
  deriving Functor

type CustomerRepoProgram = F CustomerRepoLang

addCustomer :: Customer -> CustomerRepoProgram ()
addCustomer c = liftF (AddCustomer c ())

allCustomers :: CustomerRepoProgram [Customer]
allCustomers = liftF (AllCustomers id)

findCustomerById :: CustomerId -> CustomerRepoProgram (Maybe Customer)
findCustomerById cid = liftF (FindCustomerById cid id)

runCustomerRepo :: CustomerRepoProgram a -> SqlBackend -> IO a
runCustomerRepo prog conn = foldF interpretCustomerRepo prog
  where
    interpretCustomerRepo :: CustomerRepoLang a -> IO a
    interpretCustomerRepo (AddCustomer _c a) = do
      return a

    interpretCustomerRepo (AllCustomers cont) = do
      cs <- DB.allCustomers conn
      return $ cont $ map (\(Entity _ c) ->
        customer (customerIdFromTextUnsafe $ DB.customerEntityDomainId c) (DB.customerEntityName c)) cs

    interpretCustomerRepo (FindCustomerById cid cont) = do
      m <- DB.customerByDomainId (customerIdToText cid) conn
      case m of 
        Nothing -> return $ cont Nothing
        (Just (Entity _ c)) -> do
          return $ cont $ Just $ customer cid (DB.customerEntityName c)
