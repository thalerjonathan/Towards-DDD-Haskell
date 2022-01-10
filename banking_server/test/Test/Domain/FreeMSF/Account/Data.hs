module Test.Domain.FreeMSF.Account.Data where

import           Data.Text                   as T
import           Domain.FreeMSF.Account.Api
import           Domain.FreeMSF.Account.Impl
import qualified Infrastructure.DB.Banking   as DB
import           Test.Domain.Utils

mkTestAccount :: AccountType -> Account
mkTestAccount at = account e
  where
    t = case at of
          Giro    -> DB.Giro
          Savings -> DB.Savings

    k = mkAccountKey 0
    e = DB.Entity k (DB.AccountEntity {
        DB.accountEntityOwner   = "cad6e64d-23a2-4598-8a77-17d6b8e3733b"
      , DB.accountEntityIban    = "AT12 12345 01234567890"
      , DB.accountEntityBalance = 0
      , DB.accountEntityType    = t
      })

mkTestAccountWith :: AccountType -> T.Text -> T.Text -> Double -> Account
mkTestAccountWith at owner iban bal = account e
  where
    t = case at of
          Giro    -> DB.Giro
          Savings -> DB.Savings

    k = mkAccountKey 0
    e = DB.Entity k (DB.AccountEntity {
        DB.accountEntityOwner   = owner
      , DB.accountEntityIban    = iban
      , DB.accountEntityBalance = bal
      , DB.accountEntityType    = t
      })