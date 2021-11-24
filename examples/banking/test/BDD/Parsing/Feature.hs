module BDD.Parsing.Feature
  ( Feature (..)
  , parseFeature
  ) where
  
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Feature
  = Feature String String [Scenario]
  deriving Show

data Scenario 
  = Scenario String Given
  deriving Show

data Step = Step String And
  deriving Show

data And 
  = And String And 
  | NoAnd
  deriving Show

data Given = Given Step When
  deriving Show

data When = When Step Then
  deriving Show

data Then = Then Step
  deriving Show

parseFeature :: Parser Feature
parseFeature = do
    title <- parseFeatureTitle
    (desc, ss) <- parseDescAndScenarios
    return $ Feature title desc ss
  where
    parseDescAndScenarios :: Parser (String, [Scenario])
    parseDescAndScenarios = do
      ret <- eitherP (some parseScenario) parseLine
      case ret of
        (Left ss) -> return ("", ss)
        (Right str) -> do
          (desc, ss) <- parseDescAndScenarios
          return (str ++ " " ++ desc, ss)

    parseFeatureTitle :: Parser String
    parseFeatureTitle = do
      _ <- string "Feature:"
      str <- parseLine
      return str

parseScenario :: Parser Scenario
parseScenario = do
    title <- parseScenarioTitle
    g <- parseAllSteps "Given"
    w <- parseAllSteps "When"
    t <- parseAllSteps "Then"
    
    return 
      $ Scenario title
        $ Given g 
          $ When w
            $ Then t
  where
    parseScenarioTitle :: Parser String
    parseScenarioTitle = do
      _ <- string "Scenario:"
      str <- parseLine
      return str

    parseAllSteps :: String -> Parser Step
    parseAllSteps step = do
      s  <- parseStep step
      ss <- many $ parseStep "And"
      let as = foldr (\str a -> And str a) NoAnd ss
      return $ Step s as


    parseStep :: String -> Parser String
    parseStep step = do
      _ <- string step
      hspace
      str <- many $ printChar
      space1 <|> eof
      return str

parseLine :: Parser String
parseLine = do
  hspace
  str <- many $ printChar
  hspace
  _ <- newline
  return str