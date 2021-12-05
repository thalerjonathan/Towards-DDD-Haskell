module Application.Exceptions where

import           Data.Text                     as T

data Exception
  = CustomerNotFound
  | AccountNotFound
  | InvalidAccountOperation T.Text
  deriving Show