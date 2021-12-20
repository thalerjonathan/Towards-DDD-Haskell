{-# LANGUAGE TemplateHaskell #-}
module Test.Cucumber.Generator.StepDefinition where

import           Language.Haskell.TH        (Exp, Q)
import           Language.Haskell.TH.Quote  (QuasiQuoter (..))

import           Test.Cucumber.Parsing.Step (parseGivenStep, parseThenStep,
                                             parseWhenStep)
import           Text.Megaparsec            (parse)

given_ :: QuasiQuoter
given_ = QuasiQuoter {
    quoteExp  = givenExpr
  , quotePat  = unsupported "Given" "pattern"
  , quoteType = unsupported "Given" "type"
  , quoteDec  = unsupported "Given" "declaration"
  }

when_ :: QuasiQuoter
when_ = QuasiQuoter {
    quoteExp  = whenExpr
  , quotePat  = unsupported "When" "pattern"
  , quoteType = unsupported "When" "type"
  , quoteDec  = unsupported "When" "declaration"
  }

then_ :: QuasiQuoter
then_ = QuasiQuoter {
    quoteExp  = thenExpr
  , quotePat  = unsupported "Then" "pattern"
  , quoteType = unsupported "Then" "type"
  , quoteDec  = unsupported "Then" "declaration"
  }

unsupported :: [Char] -> [Char] -> a
unsupported stepType context = error $
  "Unsupported operation: " ++  stepType ++ " can not be used in a " ++ context ++ " context."

givenExpr :: String -> Q Exp
givenExpr quote = do
  [| case parse parseGivenStep "" quote of
      (Left _err) -> error $ "Could not parse Given expression: " ++ quote
      (Right p)   -> p |]

whenExpr :: String -> Q Exp
whenExpr quote = do
  [| case parse parseWhenStep "" quote of
      (Left _err) -> error $ "Could not parse When expression: " ++ quote
      (Right p)   -> p |]

thenExpr :: String -> Q Exp
thenExpr quote = do
  [| case parse parseThenStep "" quote of
      (Left _err) -> error $ "Could not parse Then expression: " ++ quote
      (Right p)   -> p |]
