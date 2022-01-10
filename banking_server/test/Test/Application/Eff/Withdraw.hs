module Test.Application.Eff.Withdraw where

import           Application.DTO
import           Application.Eff.Banking     as Banking
import           Control.Monad.Except
import           Domain.Eff.Account
import           Domain.Types
import           Test.Application.Eff.Runner
import           Test.Application.Utils
import           Test.Domain.Eff.Account.Data
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils.Time

withdraw_eff_tests :: TestTree
withdraw_eff_tests = testGroup "Withdraw Eff Tests"
                      [ given_noaccount_when_withdraw_then_exception
                      , given_giroinrepo_when_withdraw_then_return_new_tx
                      ]

given_noaccount_when_withdraw_then_exception :: TestTree
given_noaccount_when_withdraw_then_exception = testCase "No account withdraw throws exception" $ do
  let iban   = "AT12 12345 01234567890"
      amount = 123

      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [] []

  let ret = testApplication appData (runExceptT $ Banking.withdraw iban amount)

  expectAccountNotFound ret

given_giroinrepo_when_withdraw_then_return_new_tx :: TestTree
given_giroinrepo_when_withdraw_then_return_new_tx = testCase "Withdraw from existing Account" $ do
  let iban   = "AT12 12345 01234567890"
      amount = 123
      a      = mkTestAccount Giro

      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [(Iban iban, a)] []

      txDtoExpected = TXLineDTO iban "Withdraw" "Withdraw" (-amount) now

  let ret = testApplication appData (runExceptT $ Banking.withdraw iban amount)

  case ret of
    Left e              -> assertFailure $ "Did not expect exception " ++ show e
    (Right txDtoActual) -> assertEqual "TXLineDTOs do not match!" txDtoExpected txDtoActual
