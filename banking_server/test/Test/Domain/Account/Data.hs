module Test.Domain.Account.Data where

import           Database.Persist.Sql
import           Domain.Account.Api
import           Domain.Account.Impl
import qualified Infrastructure.DB.Banking as DB

mkAccountKey :: DB.Key DB.AccountEntity
mkAccountKey =
    case keyFromValues v of
      (Left err) -> error $ show err
      (Right k)  -> k
  where
    v = [PersistInt64 0]

mkTestAccount :: AccountType -> Account
mkTestAccount at = account e
  where
    t = case at of
          Giro    -> DB.Giro
          Savings -> DB.Savings

    k = mkAccountKey
    e = DB.Entity k (DB.AccountEntity {
        DB.accountEntityOwner   = "cad6e64d-23a2-4598-8a77-17d6b8e3733b"
      , DB.accountEntityIban    = "AT12 12345 01234567890"
      , DB.accountEntityBalance = 0
      , DB.accountEntityType    = t
      })
