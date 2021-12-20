module Test.Cucumber.Parsing.Step
  ( parseStepType
  , parseGivenStep
  , parseWhenStep
  , parseThenStep
  ) where

import           Data.Void               (Void)
import           Text.Megaparsec         (Parsec, many, some, (<|>))
import           Text.Megaparsec.Char    (alphaNumChar, char, spaceChar, string)

import           Test.Cucumber.Data.Step (StepDefinition (..),
                                          StepParam (Double, Int, Str, Word),
                                          StepType (..))

type Parser = Parsec Void String


parseGivenStep :: Parser StepType
parseGivenStep = Given <$> parseStepDefinition

parseWhenStep :: Parser StepType
parseWhenStep = When <$> parseStepDefinition

parseThenStep :: Parser StepType
parseThenStep = Then <$> parseStepDefinition

parseStepType :: Parser StepType
parseStepType = parseGiven <|> parseWhen <|> parseThen
  where
    parseGiven :: Parser StepType
    parseGiven = parseGherkinStepOf "Given" Given

    parseWhen :: Parser StepType
    parseWhen = parseGherkinStepOf "When" When

    parseThen :: Parser StepType
    parseThen = parseGherkinStepOf "Then" Then

    parseGherkinStepOf :: String -> (StepDefinition -> StepType) -> Parser StepType
    parseGherkinStepOf stepIdentifier f = do
      _ <- string stepIdentifier
      f <$> parseStepDefinition

parseStepDefinition :: Parser StepDefinition
parseStepDefinition
  = (do
    _ <- many spaceChar
    t   <- parseStepText
    Text t <$> parseStepDefinition)
  <|> (do
    p   <- parseStepParam
    Param p <$> parseStepDefinition)
  <|>
    return StepEnd

parseStepText :: Parser String
parseStepText = some (alphaNumChar <|> spaceChar)

parseStepParam :: Parser StepParam
parseStepParam = do
    _ <- char '{'
    ret <- parseDouble <|> parseInt <|> parseWord <|> parseString
    _ <- char '}'
    return ret
  where
    parseDouble :: Parser StepParam
    parseDouble = parseStepParamOf "double" Double

    parseInt :: Parser StepParam
    parseInt = parseStepParamOf "int" Int

    parseWord :: Parser StepParam
    parseWord = parseStepParamOf "word" Word

    parseString :: Parser StepParam
    parseString = parseStepParamOf "string" Str

    parseStepParamOf :: String -> StepParam -> Parser StepParam
    parseStepParamOf paramIdentifier p = do
      _ <- string paramIdentifier
      return p
