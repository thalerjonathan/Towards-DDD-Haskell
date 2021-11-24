module BDD.Parsing.Step 
  ( parseStepType
  ) where
  
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import BDD.Data.Step

type Parser = Parsec Void String

parseStepType :: Parser StepType
parseStepType = parseGivenStep <|> parseWhenStep <|> parseThenStep
  where
    parseGivenStep :: Parser StepType
    parseGivenStep = parseGherkinStepOf "Given" Given

    parseWhenStep :: Parser StepType
    parseWhenStep = parseGherkinStepOf "When" When

    parseThenStep :: Parser StepType
    parseThenStep = parseGherkinStepOf "Then" Then

    parseGherkinStepOf :: String -> (StepDefinition -> StepType) -> Parser StepType
    parseGherkinStepOf stepIdentifier f = do
      _ <- string stepIdentifier
      e <- parseStepDefinition
      return $ f e

    parseStepDefinition :: Parser StepDefinition
    parseStepDefinition 
      = (do
        t   <- parseStepText
        --ret <- parseStepDefinition
        return $ Text t StepEnd)
      <|> (do
        p   <- parseStepParam
        --ret <- parseStepDefinition
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