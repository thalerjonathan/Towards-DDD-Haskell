module Application.BankingDomain where

import           Application.DTO
import           Application.Exceptions
import           Data.Maybe
import           Data.Text                 as T
import           Data.UUID
import           Domain.AccountLang
import           Domain.AccountRepository
import           Domain.Application
import           Domain.Customer
import           Domain.CustomerRepository

-- TODO: create customer
-- TODO: create account

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
  ma <- runRepo $ accountRepo $ findAccountByIban (Iban ibanStr)
  case ma of
    Nothing -> return $ Left AccountNotFound
    (Just a) -> do
      ret <- runAggregate $ accountAggregate $ accountToDTO a
      return $ Right ret

deposit :: T.Text
        -> Double
        -> Application (Either Exception TXLineDTO)
deposit i amount = do
  ma <- runRepo $ accountRepo $ findAccountByIban (Iban i)
  case ma of
    Nothing -> return $ Left AccountNotFound
    (Just a) -> do
      (_, ret, _) <- runAggregate $ accountAggregate $ Domain.AccountLang.deposit a amount
      case ret of
        (Just (DepositResult (Left err))) -> return $ Left $ InvalidAccountOperation err
        (Just (DepositResult (Right tx))) -> return $ Right $ txLineToDTO tx
        _ -> error "Unexpected result of Domain.Account.deposit"

withdraw :: T.Text
         -> Double
         -> Application (Either Exception TXLineDTO)
withdraw i amount = do
  ma <- runRepo $ accountRepo $ findAccountByIban (Iban i)
  case ma of
    Nothing -> return $ Left AccountNotFound
    (Just a) -> do
      (_, ret, _) <- runAggregate $ accountAggregate $ Domain.AccountLang.withdraw a amount
      case ret of
        (Just (WithdrawResult (Left err))) -> return $ Left $ InvalidAccountOperation err
        (Just (WithdrawResult (Right tx))) -> return $ Right $ txLineToDTO tx
        _ -> error "Unexpected result of Domain.Account.withdraw"

customerToDetailsDTO :: Customer -> CustomerProgram CustomerDetailsDTO
customerToDetailsDTO c = do
  cname <- getName c
  cid   <- getDomainId c

  return $ CustomerDetailsDTO
    { customerDetailsId   = T.pack $ show cid
    , customerDetailsName = cname
    }

accountToDTO :: Account -> AccountProgram AccountDTO
accountToDTO  a 
  = AccountDTO 
      <$> accountToDetailsDTO a 
      <*> (Prelude.map txLineToDTO <$> getTXLines a)

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

txLineToDTO :: TXLine -> TXLineDTO
txLineToDTO (TXLine a (Iban i) n ref t) = TXLineDTO
  { txLineIban      = i
  , txLineName      = n
  , txLineReference = ref
  , txLineAmount    = a
  , txLineTime      = t
  } 