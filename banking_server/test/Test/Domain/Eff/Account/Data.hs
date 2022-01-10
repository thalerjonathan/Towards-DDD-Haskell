module Test.Domain.Eff.Account.Data where

import           Data.Text          as T
import           Domain.Eff.Account
import           Domain.Types
import           Test.Domain.Utils

mkTestAccount :: AccountType -> Account
mkTestAccount at = Account aid owner i bal at
  where
    aid   = mkAccountKey 0
    owner = customerIdFromTextUnsafe "cad6e64d-23a2-4598-8a77-17d6b8e3733b"
    i     = Iban "AT12 12345 01234567890"
    bal   = 0

mkTestAccountWith :: AccountType -> T.Text -> T.Text -> Double -> Account
mkTestAccountWith at ownerStr ibanStr bal = Account aid owner i bal at
  where
    aid   = mkAccountKey 0
    owner = customerIdFromTextUnsafe ownerStr
    i     = Iban ibanStr
