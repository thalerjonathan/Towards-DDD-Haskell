module Test.Application.Utils where

import           Application.Exceptions
import           Data.Text
import           Test.Tasty.HUnit

expectAccountNotFound :: Show a => Either Exception a -> Assertion
expectAccountNotFound (Left AccountNotFound) = return ()
expectAccountNotFound ret                      = assertFailure $ "Expected AccountNotFound but got '" ++ show ret ++ "'"

expectCustomerNotFound :: Show a => Either Exception a -> Assertion
expectCustomerNotFound (Left CustomerNotFound) = return ()
expectCustomerNotFound ret                     = assertFailure $ "Expected CustomerNotFound but got '" ++ show ret ++ "'"

expectInvalidAccountOp :: Show a => Either Exception a -> Text -> Assertion
expectInvalidAccountOp (Left (InvalidAccountOperation err)) expectedErr = assertEqual "Transfer Exception not matching" err expectedErr
expectInvalidAccountOp ret _                                            = assertFailure $ "Expected InvalidAccountOperation but got '" ++ show ret ++ "'"
