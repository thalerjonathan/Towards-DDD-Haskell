module Application.Deposit where

import           Test.Tasty
import           Test.Tasty.HUnit

import Application.BankingDomain

deposit_tests :: TestTree
deposit_tests = testGroup "Deposit Tests"
                      [ given_repository_when_deposit_then_return_new_tx
                      ]

given_repository_when_deposit_then_return_new_tx :: TestTree
given_repository_when_deposit_then_return_new_tx = testCase "Deposit into existing Account" $ do
  let i      = Iban "AT12 12345 01234567890"
      amount = 123

  let ret = testApplication 
              (Banking.deposit iban amount))

  return ()

testApplication :: Application a 
                -> (Repository a -> a) 
                -> (Aggregate a -> a)
                -> a
testApplication prog repo agg = foldF interpret prog
  where
    interpret :: ApplicationLang a -> a
    interpret (RunRepo r f)  = do
      f <$> repo r conn
    interpret (RunAggregate a f) = do
      f <$> agg a conn

interpretRepo :: Repository a -> a
interpretRepo (AccountRepo r)  = runAccountRepo r
interpretRepo (CustomerRepo r) = runCustomerRepo r

interpretAggregate :: Aggregate a -> a
interpretAggregate (CustomerAggregate a) = runCustomerAggregate a
interpretAggregate (AccountAggregate a)  = runAccountAggregate a conn



testAccountRepo :: AccountRepoProgram a -> a
testAccountRepo prog = foldF interpret prog
  where
    interpret :: AccountRepoLang a -> IO a
    interpret (AddAccount _a ret) = do
      return ret
    interpret (FindAccountsForOwner (CustomerId cDomainId) cont) = do
      as <- DB.accountsOfCustomer (toText cDomainId) conn
      let aggs = map (\e -> account e) as
      return $ cont aggs

    interpret (FindAccountByIban (Iban i) cont) = do
      m <- DB.accountByIban i conn
      case m of 
        Nothing  -> return (cont Nothing)
        (Just e) -> return (cont $ Just $ account e)
