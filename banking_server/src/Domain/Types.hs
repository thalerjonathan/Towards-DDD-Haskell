module Domain.Types where

import           Data.Maybe
import qualified Data.Text    as T
import           Data.UUID

newtype CustomerId = CustomerId UUID deriving Eq

instance Show CustomerId where
  show (CustomerId cid) = show cid

newtype Iban = Iban T.Text deriving Show
type Money   = Double

customerIdFromTextUnsafe :: T.Text -> CustomerId
customerIdFromTextUnsafe = CustomerId . fromJust . fromText

customerIdToText :: CustomerId -> T.Text
customerIdToText (CustomerId cid) = toText cid
