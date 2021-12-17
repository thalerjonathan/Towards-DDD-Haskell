module Test.Domain.Account.Runner where

import           Control.Monad.Free.Church
import           Control.Monad.State
import           Data.Time
import           Domain.Account.Api
import           Domain.Types
import           Test.Utils.Time

testAccountProgram:: AccountProgram a -> a
testAccountProgram prog = ret
  where
    now = mkUTCTime 2021 03 01 0 0 0
    (ret, (_mBal, _mTxs)) = runState (runAccountPure prog [] now) (Nothing, Nothing)

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
