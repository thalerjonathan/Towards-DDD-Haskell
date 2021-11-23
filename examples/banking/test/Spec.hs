-- {-# LANGUAGE OverloadedStrings          #-}
module Main where

-- import Text.Megaparsec
-- import Data.Text
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

main :: IO ()
main = parseTest parseGherkinStep "Given my Giro account has a balance of {double} euro"

type Parser = Parsec Void String

data GherkinStep 
  = Given Step
  | When Step
  | Then Step
  deriving Show

data Step 
  = Text String Step 
  | Param StepParam Step 
  | StepEnd
  deriving Show

data StepParam 
  = Word 
  | Int 
  | Double 
  deriving Show

parseGherkinStep :: Parser GherkinStep
parseGherkinStep = parseGivenStep <|> parseWhenStep <|> parseThenStep
  where
    parseGivenStep :: Parser GherkinStep
    parseGivenStep = parseGherkinStepOf "Given" Given

    parseWhenStep :: Parser GherkinStep
    parseWhenStep = parseGherkinStepOf "When" When

    parseThenStep :: Parser GherkinStep
    parseThenStep = parseGherkinStepOf "Then" Then

    parseGherkinStepOf :: String -> (Step -> GherkinStep) -> Parser GherkinStep
    parseGherkinStepOf stepIdentifier f = do
      _ <- string stepIdentifier
      e <- parseStep
      return $ f e

    parseStep :: Parser Step
    parseStep 
      = (do
        t   <- parseStepText
        --ret <- parseStep
        return $ Text t StepEnd)
      <|> (do
        p   <- parseStepParam
        --ret <- parseStep
        return $ Param p StepEnd) 
      <|>
        (return StepEnd)

    parseStepText :: Parser String
    parseStepText = many (alphaNumChar <|> spaceChar)

    parseStepParam :: Parser StepParam
    parseStepParam = do
        _ <- char '{'
        ret <- parseDouble <|> parseInt <|> parseWord
        _ <- char '}'
        return ret
      where
        parseDouble :: Parser StepParam
        parseDouble = parseStepParamOf "double" Double

        parseInt :: Parser StepParam
        parseInt = parseStepParamOf "int" Int

        parseWord :: Parser StepParam
        parseWord = parseStepParamOf "word" Word

        parseStepParamOf :: String -> StepParam -> Parser StepParam
        parseStepParamOf paramIdentifier p = do
          _ <- string paramIdentifier
          return p

{-
parseGiven :: Parser String
parseGiven = do
  _ <- string "Given"
  _ <- many $ alphaNumChar <|> spaceChar
  _ <- char '{'
  str <- (string "double" <|> string "word" <|> string "int")
  _ <- char '}'
  return str


data Gherkin 
  = Given Step Gherkin
  | When Step Gherkin
  | Then Step Gherkin
  | GherkinEnd

data Step = Text Text Step | Word Text Step | Int Int Step | Double Double Step | StepEnd

--  Scenario: Deposit money into a Giro account
scenario1 :: Gherkin 
scenario1 = Given given1 (When when1 (Then then1 GherkinEnd))

given1 :: Step 
given1 = Text "my Giro account has a balance of" (Double 1234.56 StepEnd)

when1 :: Step 
when1 = Text "I deposit" (Double 567.89 (Text "into my account" StepEnd))

then1 :: Step 
then1 = Text "I should have a balance of" (Double 1802.45 (Text "in my account" StepEnd))
-}

-- Given my Giro account has a balance of 1234.56
-- Given my Giro account has a balance of {double}
givenStep :: Double -> IO ()
givenStep _balance = undefined

-- When I deposit 567.89 into my account
-- When I deposit {double} into my account
whenStep :: Double -> IO ()
whenStep _balance = undefined

-- Then I should have a balance of 1802.45 in my account
-- Then I should have a balance of {double} in my account
thenStep :: Double -> IO ()
thenStep _balance = undefined