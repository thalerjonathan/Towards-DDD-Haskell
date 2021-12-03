module Application.BankingNew where

import           Application.DTO
import           Application.Exceptions
import           Data.Maybe
import           Data.Text                 as T
import           Data.UUID
import           Domain.Account
import           Domain.AccountRepository
import           Domain.Application
import           Domain.Customer
import           Domain.CustomerRepository

getAllCustomers :: Application [CustomerDetailsDTO]
getAllCustomers = do
    cs <- runRepo $ customerRepo $ allCustomers
    dtos <- runAggregate $ customerAggregate $ mapM customerToDetailsDTO cs
    return dtos

getCustomer :: T.Text -> Application (Either Exception CustomerDTO)
getCustomer cIdStr = do
    let cid = CustomerId $ fromJust $ fromText cIdStr

    mc <- runRepo $ customerRepo $ findCustomerById cid
    case mc of
      Nothing -> return $ Left CustomerNotFound
      (Just c) -> do
        as <- runRepo $ accountRepo $ findAccountsForOwner cid

        custDetails <- runAggregate $ customerAggregate $ customerToDetailsDTO c
        asDetails   <- runAggregate $ accountAggregate $ mapM accountToDetailsDTO as

        let dto = CustomerDTO {
            customerDetails        = custDetails
          , customerAccountDetails = asDetails
          }

        return $ Right dto

getAccount :: T.Text -> Application (Either Exception AccountDTO)
getAccount ibanStr = do
  let i = Iban ibanStr

  ma <- runRepo $ accountRepo $ findAccountByIban i
  case ma of
    Nothing -> return $ Left AccountNotFound
    (Just a) -> do
      -- txs <- DB.txLinesOfAccount aid conn
      asDetails <- runAggregate $ accountAggregate $ accountToDetailsDTO a

      let dto = AccountDTO {
        accountDetails = asDetails
      , accountTXLines = []
      }

      return $ Right dto

customerToDetailsDTO :: Customer -> CustomerProgram CustomerDetailsDTO
customerToDetailsDTO c = do
  cname <- getName c
  cid <- getDomainId c

  return $ CustomerDetailsDTO
    { customerDetailsId   = T.pack $ show cid
    , customerDetailsName = cname
    }

accountToDetailsDTO :: Account -> AccountProgram AccountDetailsDTO
accountToDetailsDTO a = do
  (Iban i) <- getIban a
  b        <- getBalance a
  accType  <- getType a

  return $ AccountDetailsDTO
    { accountDetailIban    = i
    , accountDetailBalance = b
    , accountDetailType    = T.pack $ show $ accType
    }
