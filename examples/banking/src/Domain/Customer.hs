{-# LANGUAGE Arrows             #-}
module Domain.Customer where

import Data.UUID
import Control.Monad.Free (Free (Pure, Free), liftF)
import Data.Text (Text)
import Data.MonadicStreamFunction (MSF, returnA, arrM, feedback)
import Data.MonadicStreamFunction.InternalCore (unMSF)

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

type CustomerProgram = Free CustomerLang

data CustomerState 
  = CustomerState CustomerId deriving Show

type Customer = MSF CustomerProgram CustomerCommand (Maybe CustomerCommandResult)

execCommand :: Customer -> CustomerCommand -> IO (Customer, Maybe CustomerCommandResult)
execCommand a cmd = do
  (ret, a') <- interpret (unMSF a cmd)
  return (a', ret)

customer :: CustomerId -> Customer
customer cid = feedback s0 (proc (cmd, s) -> do
    ret <- arrM (handleCommand cid) -< cmd
    returnA -< (ret, s))
  where
    s0 = CustomerState cid

readName :: CustomerProgram Text
readName = liftF (ReadName id)

handleCommand :: CustomerId 
              -> CustomerCommand 
              -> CustomerProgram (Maybe CustomerCommandResult)
handleCommand  cid GetCustomerId = do
  return $ Just (ReturnCustomerId cid)
handleCommand  _ GetName = do
  name <- readName
  return $ Just (ReturnName name)
  
interpret :: CustomerProgram a -> IO a
interpret (Pure a) = return a
interpret (Free (ReadName contF)) = do
  let name = "Jonathan Thaler"
  interpret (contF name)