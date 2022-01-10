{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-} -- for more convenience defining mocks
{-# OPTIONS_GHC -fno-warn-incomplete-patterns     #-} -- for more convenience defining mocks
{-# LANGUAGE LambdaCase #-}
module Test.Application.FreeMSF.Deposit where

import           Application.FreeMSF.Banking as Banking
import           Application.DTO
import           Control.Monad.Except
import           Domain.FreeMSF.Account.Api
import           Domain.FreeMSF.Account.Repository
import           Domain.Types
import           Test.Application.FreeMSF.Runner
import           Test.Application.Utils
import           Test.Domain.FreeMSF.Account.Data
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils.Time

deposit_freemsf_tests :: TestTree
deposit_freemsf_tests = testGroup "Deposit FreeMSF Tests"
                      [ given_noaccount_when_deposit_then_exception
                      , given_giroinrepo_when_deposit_then_return_new_tx
                      ]

given_noaccount_when_deposit_then_exception :: TestTree
given_noaccount_when_deposit_then_exception = testCase "No account deposit throws exception" $ do
  let iban   = "AT12 12345 01234567890"
      amount = 123

  let accountRepoMock = (\(FindAccountByIban _ cont) -> return $ cont Nothing)

  let ret = testApplication
              (runExceptT $ Banking.deposit iban amount)
              accountRepoMock
              undefined
              undefined

  expectAccountNotFound ret

given_giroinrepo_when_deposit_then_return_new_tx :: TestTree
given_giroinrepo_when_deposit_then_return_new_tx = testCase "Deposit into existing Account" $ do
  let iban   = "AT12 12345 01234567890"
      amount = 123
      a      = mkTestAccount Giro
      now    = mkUTCTime 2021 03 01 0 0 0

      txDtoExpected = TXLineDTO iban "Deposit" "Deposit" amount now

  -- NOTE: could check if _iban' and iban are same, but we don't care for now
  let accRepoMock = \(FindAccountByIban (Iban _iban') cont) -> return $ cont (Just a)
  let accAggMock =  \case
                      PersistTXLine _ m i name ref cont -> return $ cont $ TXLine m i name ref now
                      UpdateBalance _ _ ret -> return ret

  let ret = testApplication
              (runExceptT $ Banking.deposit iban amount)
              accRepoMock
              undefined
              accAggMock

  case ret of
    Left e              -> assertFailure $ "Did not expect exception " ++ show e
    (Right txDtoActual) -> assertEqual "TXLineDTOs do not match!" txDtoExpected txDtoActual
