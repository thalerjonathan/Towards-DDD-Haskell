module Application.Exceptions where

import           Control.Monad.Except
import           Data.Text            as T

data Exception
  = CustomerNotFound
  | AccountNotFound
  | InvalidAccountOperation T.Text
  deriving Show

throwJust :: Monad m => (a -> ex) -> m (Maybe a) -> ExceptT ex m ()
throwJust f act = do
  ret <- lift act
  case ret of
    Nothing  -> return ()
    (Just a) -> throwError $ f a

throwMaybe :: Monad m => ex -> m (Maybe a) -> ExceptT ex m a
throwMaybe ex act = do
  ret <- lift act
  case ret of
    Nothing  -> throwError ex
    (Just a) -> return a

throwMaybeAction :: Monad m => m (Maybe a) -> m ex -> ExceptT ex m a
throwMaybeAction act consume = do
  ret <- lift act
  case ret of
    Nothing  -> do
      ex <- lift consume
      throwError ex
    (Just a) -> return a
