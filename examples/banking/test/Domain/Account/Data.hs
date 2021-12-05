module Domain.Account.Data where

import           Data.Fixed
import           Data.Time
import           Database.Persist.Sql
import           Domain.Account.Api
import           Domain.Account.Impl
import qualified Infrastructure.DB.Banking as DB

-- https://wiki.haskell.org/Time
-- https://williamyaoh.com/posts/2019-09-16-time-cheatsheet.html
mkUTCTime :: Integer
          -> Int
          -> Int
          -> Int
          -> Int
          -> Pico
          -> UTCTime
mkUTCTime year mon day hour mn sec =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour mn sec))

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
