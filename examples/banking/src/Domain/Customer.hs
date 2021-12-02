{-# LANGUAGE Arrows #-}
module Domain.Customer where

import           Control.Monad.Free.Church
import           Data.MonadicStreamFunction              (MSF, arrM, feedback,
                                                          returnA)
import           Data.MonadicStreamFunction.InternalCore (unMSF)
import           Data.Text                               as T
import           Data.UUID

newtype CustomerId = CustomerId UUID deriving Show

data CustomerCommand
  = GetCustomerId
  | GetName
  deriving Show

data CustomerCommandResult
  = ReturnCustomerId CustomerId
  | ReturnName Text
  deriving Show

data CustomerLang a
  = ReadName (Text -> a)
  deriving Functor

type CustomerProgram = F CustomerLang

data CustomerState
  = CustomerState CustomerId deriving Show

type Customer = MSF CustomerProgram CustomerCommand (Maybe CustomerCommandResult)

customer :: CustomerId -> T.Text -> Customer
customer cid cName = feedback s0 (proc (cmd, s) -> do
    ret <- arrM (handleCommand cid cName) -< cmd
    returnA -< (ret, s))
  where
    s0 = CustomerState cid

readName :: CustomerProgram Text
readName = liftF (ReadName id)

getName :: Customer -> CustomerProgram T.Text
getName c = do
  (ret, _) <- unMSF c GetName
  case ret of
    (Just (ReturnName n)) -> return n
    _                     -> error "unexpected return in customer"

handleCommand :: CustomerId
              -> T.Text
              -> CustomerCommand
              -> CustomerProgram (Maybe CustomerCommandResult)
handleCommand cid _ GetCustomerId = do
  return $ Just (ReturnCustomerId cid)
handleCommand _ cname GetName = do
  return $ Just (ReturnName cname)

execCommand :: Customer -> CustomerCommand -> IO (Customer, Maybe CustomerCommandResult)
execCommand a cmd = do
  (ret, a') <- runCustomerAggregate (unMSF a cmd)
  return (a', ret)

runCustomerAggregate :: CustomerProgram a -> IO a
runCustomerAggregate = foldF interpret
  where
    interpret :: CustomerLang a -> IO a
    interpret (ReadName cont) = do
      let name = "Jonathan Thaler"
      return $ cont name
