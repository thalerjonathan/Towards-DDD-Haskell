{-# LANGUAGE TemplateHaskell #-}
module Test.Cucumber.Generator.StepDefinition where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- import Test.Cucumber.Data.Step
--import Test.Cucumber.Parsing.Step
--import Text.Megaparsec hiding (State)

given :: QuasiQuoter
given = QuasiQuoter {
    quoteExp  = givenExpr
  , quotePat  = unsupported "Given" "pattern"
  , quoteType = unsupported "Given" "type"
  , quoteDec  = unsupported "Given" "declaration"
  }

-- unsupported :: MonadFail m => String -> String -> m a
unsupported :: [Char] -> [Char] -> a
unsupported stepType context = error $
    "Unsupported operation: " ++  stepType ++ " can not be used in a " ++ context ++ " context."

givenExpr :: String -> Q Exp
givenExpr quote = do
  reportError $ "givenExpr: " ++ quote
  return $ VarE $ mkName "foobar"
  -- [| Given (Text "my Giro account has a balance of" (Param Double StepEnd)) |]
  -- case parse parseStepType "" quote of 
  --     (Left _err) -> error $ "Could not parse Given expression: " ++ quote
  --     (Right _)  -> return $ VarE $ mkName "foobar" -- [| return p |]