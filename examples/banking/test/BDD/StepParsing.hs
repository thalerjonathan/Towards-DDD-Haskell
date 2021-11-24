module BDD.StepParsing where
  
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

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