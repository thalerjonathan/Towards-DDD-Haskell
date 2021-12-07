module Application.Exceptions where

import           Application.Layer
import           Control.Monad.Except
import           Data.Text            as T

data Exception
  = CustomerNotFound
  | AccountNotFound
  | InvalidAccountOperation T.Text
  deriving Show

type Application          = ApplicationLayer
type ApplicationExcept ex = ExceptT ex Application

throwJust :: (a -> ex) -> Application (Maybe a) -> ApplicationExcept ex ()
throwJust f act = do
  ret <- lift $ act
  case ret of
    Nothing  -> return ()
    (Just a) -> throwError $ f a

throwMaybe :: ex -> Application (Maybe a) -> ApplicationExcept ex a
throwMaybe ex act = do
  ret <- lift $ act
  case ret of
    Nothing  -> throwError ex
    (Just a) -> return a

throwMaybeAction :: Application (Maybe a) -> Application ex -> ApplicationExcept ex a
throwMaybeAction act consume = do
  ret <- lift $ act
  case ret of
    Nothing  -> do
      ex <- lift $ consume
      throwError ex
    (Just a) -> return a