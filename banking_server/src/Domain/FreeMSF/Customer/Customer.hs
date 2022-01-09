{-# LANGUAGE Arrows #-}
module Domain.FreeMSF.Customer.Customer where

import           Control.Monad.Identity
import           Data.MonadicStreamFunction              (MSF, arrM, feedback,
                                                          returnA)
import           Data.MonadicStreamFunction.InternalCore (unMSF)
import           Data.Text                               as T
import           Domain.FreeMSF.Types

data CustomerCommand
  = GetDomainId
  | GetName
  deriving Show

data CustomerCommandResult
  = ReturnDomainId CustomerId
  | ReturnName Text
  deriving Show

type CustomerProgram = Identity

data CustomerState
  = CustomerState CustomerId deriving Show

type Customer = MSF CustomerProgram CustomerCommand CustomerCommandResult

customer :: CustomerId -> T.Text -> Customer
customer cid cName = feedback s0 (proc (cmd, s) -> do
    ret <- arrM (handleCommand cid cName) -< cmd
    returnA -< (ret, s))
  where
    s0 = CustomerState cid

getName :: Customer -> CustomerProgram T.Text
getName c = do
  (ret, _) <- unMSF c GetName
  case ret of
    (ReturnName n) -> return n
    _              -> error "unexpected return in customer"

getDomainId :: Customer -> CustomerProgram CustomerId
getDomainId c = do
  (ret, _) <- unMSF c GetDomainId
  case ret of
    (ReturnDomainId n) -> return n
    _                  -> error "unexpected return in customer"

handleCommand :: CustomerId
              -> T.Text
              -> CustomerCommand
              -> CustomerProgram CustomerCommandResult
handleCommand cid _ GetDomainId = do
  return $ ReturnDomainId cid
handleCommand _ cname GetName = do
  return $ ReturnName cname

runCustomerAggregate :: CustomerProgram a -> a
runCustomerAggregate act = runIdentity act