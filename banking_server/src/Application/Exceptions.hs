module Application.Exceptions where

import           Control.Monad.Except
import           Data.Text            as T

data Exception
  = CustomerNotFound
  | AccountNotFound
  | InvalidAccountOperation T.Text
  deriving Show

guardWith :: Monad m => Maybe ex -> ExceptT ex m ()
guardWith Nothing   = return ()
guardWith (Just ex) = throwError ex 

guardWith_ :: MonadError ex m => Maybe ex -> m ()
guardWith_ Nothing   = return ()
guardWith_ (Just ex) = throwError ex 

guardWithM :: Monad m => m (Maybe a) -> (a -> ex) -> ExceptT ex m ()
guardWithM act f = do
  ret <- lift act
  case ret of
    Nothing  -> return ()
    (Just a) -> throwError $ f a

guardWithM_ :: MonadError ex m => m (Maybe a) -> (a -> ex) -> m ()
guardWithM_ act f = do
  ret <- act
  case ret of
    Nothing  -> return ()
    (Just a) -> throwError $ f a

tryMaybe :: Monad m => m (Maybe a) -> ex -> ExceptT ex m a
tryMaybe act ex = do
  ret <- lift act
  case ret of
    Nothing  -> throwError ex
    (Just a) -> return a

tryMaybe_ :: MonadError ex m => m (Maybe a) -> ex -> m a
tryMaybe_ act ex = do
  ret <- act
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

tryMaybeM_ :: MonadError ex m => m (Maybe a) -> m ex -> m a
tryMaybeM_ act consume = do
  ret <- act
  case ret of
    Nothing  -> do
      ex <- consume
      throwError ex
    (Just a) -> return a
