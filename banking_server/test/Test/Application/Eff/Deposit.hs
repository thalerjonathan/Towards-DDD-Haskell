module Test.Application.Eff.Deposit where

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

deposit_eff_tests :: TestTree
deposit_eff_tests = testGroup "Deposit Eff Tests"
                      [ given_noaccount_when_deposit_then_exception
                      , given_giroinrepo_when_deposit_then_return_new_tx
                      ]

given_noaccount_when_deposit_then_exception :: TestTree
given_noaccount_when_deposit_then_exception = testCase "No account deposit throws exception" $ do
  let iban    = "AT12 12345 01234567890"
      amount  = 123

      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [] []
      
  let ret = testApplication appData (runExceptT $ Banking.deposit iban amount)

  expectAccountNotFound ret

given_giroinrepo_when_deposit_then_return_new_tx :: TestTree
given_giroinrepo_when_deposit_then_return_new_tx = testCase "Deposit into existing Account" $ do
  let iban    = "AT12 12345 01234567890"
      amount  = 123
      a       = mkTestAccount Giro

      now     = mkUTCTime 2021 03 01 0 0 0
      appData = mkPureAppData now [(Iban iban, a)] []

      txDtoExpected = TXLineDTO iban "Deposit" "Deposit" amount now

  let ret = testApplication appData (runExceptT $ Banking.deposit iban amount)

  case ret of
    Left e              -> assertFailure $ "Did not expect exception " ++ show e
    (Right txDtoActual) -> assertEqual "TXLineDTOs do not match!" txDtoExpected txDtoActual
