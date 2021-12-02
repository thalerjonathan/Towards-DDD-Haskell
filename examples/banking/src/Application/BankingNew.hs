module Application.BankingNew where

import           Application.DTO
import           Data.Maybe
import           Data.Text                 as T
import           Data.UUID
import           Domain.Account
import           Domain.AccountRepository
import           Domain.Application
import           Domain.Customer
import           Domain.CustomerRepository

data Exception
  = CustomerNotFound
  | AccountNotFound
  | InvalidAccountOperation T.Text
  deriving Show

getAllCustomers :: Application [CustomerDetailsDTO]
getAllCustomers = do
    cs <- runRepo $ customerRepo $ allCustomers
    dtos <- runAggregate $ customerAggregate $ (mapM customerToDetailsDTO cs)
    return dtos
  where
    customerToDetailsDTO :: Customer -> CustomerProgram CustomerDetailsDTO
    customerToDetailsDTO c = do
      let cId = ""

      cname <- getName c

      return $ CustomerDetailsDTO
        { customerDetailsId   = cId
        , customerDetailsName = cname
        }


getCustomer :: T.Text -> Application (Either Exception CustomerDTO)
getCustomer cIdStr = do
  let cid = CustomerId $ fromJust $ fromText cIdStr

  mc <- runRepo $ customerRepo $ findCustomerById cid
  case mc of
    Nothing -> return $ Left CustomerNotFound
    (Just _c) -> do
      -- as <- DB.accountsOfCustomer cid conn
      -- return $ Right $ customerEntityToDTO c as
      return $ Left CustomerNotFound

getAccount :: T.Text -> Application (Either Exception AccountDTO)
getAccount ibanStr = do
  let i = Iban ibanStr

  ma <- runRepo $ accountRepo $ findAccountByIban i
  case ma of
    Nothing -> return $ Left AccountNotFound
    (Just _acc) -> do
      -- txs <- DB.txLinesOfAccount aid conn
      -- return $ Right $ accountEntityToDTO a txs
      return $ Left AccountNotFound
