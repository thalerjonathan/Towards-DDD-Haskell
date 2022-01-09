module Domain.Eff.Customer.Repository where

class Monad m => CustomerRepo where
  addCustomer      :: CustomerId -> T.Text -> m Customer
  allCustomers     :: m [Customer]
  findCustomerById :: CustomerId -> m (Maybe Customer)

instance CustomerRepo AppCtx where
  addCustomer :: CustomerId -> T.Text -> m Customer
  addCustomer _cid _name = undefined

  allCustomers :: m [Customer]
  allCustomers = return []

  findCustomerById :: CustomerId -> m (Maybe Customer)
  findCustomerById _cid = return Nothing