module Domain.Account.Tests where

import           Control.Monad.Free.Church
import           Control.Monad.State
import           Data.Fixed
import           Data.Time
import           Database.Persist.Sql
import           Domain.Account.Api
import           Domain.Account.Impl
import           Domain.Types
import qualified Infrastructure.DB.Banking as DB
import           Test.Tasty
import           Test.Tasty.HUnit

account_tests :: TestTree
account_tests = testGroup "Account Tests"
                  [ deposit_tests
                  ]

deposit_tests :: TestTree
deposit_tests = testCase "Deposit Scenarios" $ do
  let k = mkAccountKey
  let e = DB.Entity k (DB.AccountEntity {
      DB.accountEntityOwner   = "cad6e64d-23a2-4598-8a77-17d6b8e3733b"
    , DB.accountEntityIban    = "AT12 12345 01234567890"
    , DB.accountEntityBalance = 0
    , DB.accountEntityType    = DB.Giro
    })
  let a = account e
  let now = mkUTCTime 2021 03 01 0 0 0

  let (bal, (_mBal, _mTxs)) = runState (runAccountPure (getBalance a) [] now) (Nothing, Nothing)

  assertEqual "Balance should be 0" bal 1

mkAccountKey :: DB.Key DB.AccountEntity
mkAccountKey =
    case keyFromValues v of
      (Left err) -> error $ show err
      (Right k)  -> k
  where
    v = [PersistInt64 0]

runAccountPure :: AccountProgram a
               -> [TXLine]
               -> UTCTime
               -> State (Maybe Money, Maybe TXLine) a
runAccountPure prog txs t = foldF interpret prog
  where
    interpret :: AccountLang a -> State (Maybe Money, Maybe TXLine) a
    interpret (FetchTXLines _ cont)  = do
      return (cont txs)

    interpret (PersistTXLine _ m iban name ref cont) = do
      let tx = TXLine m iban name ref t
      modify (\(bal, _) -> (bal, Just tx))
      return (cont tx)

    interpret (UpdateBalance _ m a) = do
      modify (\(_, tx) -> (Just m, tx))
      return a

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
