module Test.Domain.Utils where

import           Database.Persist.Sql
import           GHC.Int
import qualified Infrastructure.DB.Banking as DB

mkAccountKey :: Int64 -> DB.Key DB.AccountEntity
mkAccountKey n =
    case keyFromValues v of
      (Left err) -> error $ show err
      (Right k)  -> k
  where
    v = [PersistInt64 n]

unsafeFromRight :: Either l r -> r
unsafeFromRight (Left _)  = error "Expected right value in Either!"
unsafeFromRight (Right r) = r