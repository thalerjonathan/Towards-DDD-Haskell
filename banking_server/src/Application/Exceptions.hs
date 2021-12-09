module Application.Exceptions where

import           Control.Monad.Except
import           Data.Text            as T

data Exception
  = CustomerNotFound
  | AccountNotFound
  | InvalidAccountOperation T.Text
  deriving Show

guardJust :: Monad m => Maybe ex -> ExceptT ex m ()
guardJust Nothing   = return ()
guardJust (Just ex) = throwError ex 

tryJust :: Monad m => m (Maybe a) -> (a -> ex) -> ExceptT ex m ()
tryJust act f = do
  ret <- lift act
  case ret of
    Nothing  -> return ()
    (Just a) -> throwError $ f a

tryMaybe :: Monad m => m (Maybe a) -> ex -> ExceptT ex m a
tryMaybe act ex = do
  ret <- lift act
  case ret of
    Nothing  -> throwError ex
    (Just a) -> return a

tryMaybeM :: Monad m => m (Maybe a) -> m ex -> ExceptT ex m a
tryMaybeM act consume = do
  ret <- lift act
  case ret of
    Nothing  -> do
      ex <- lift consume
      throwError ex
    (Just a) -> return a
