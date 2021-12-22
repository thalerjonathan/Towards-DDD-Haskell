module Test.Cucumber.Parsing.Gherkin
  ( parseGherkin
  ) where

import           Data.Void                  (Void)
import           Text.Megaparsec            (MonadParsec (eof), Parsec, eitherP,
                                             many, some, (<|>))
import           Text.Megaparsec.Char       (hspace, newline, printChar, space1,
                                             string)

import           Test.Cucumber.Data.Gherkin (And (And, NoAnd), Feature (..),
                                             Given (G), Scenario (..),
                                             Step (..), Then (T), When (W))

type Parser = Parsec Void String

parseGherkin :: Parser Feature
parseGherkin = do
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
      parseLine

parseScenario :: Parser Scenario
parseScenario = do
    title <- parseScenarioTitle
    g <- parseAllSteps "Given"
    w <- parseAllSteps "When"
    t <- parseAllSteps "Then"

    return
      $ Scenario title
        $ G g
          $ W w
            $ T t
  where
    parseScenarioTitle :: Parser String
    parseScenarioTitle = do
      _ <- string "Scenario:"
      parseLine

    parseAllSteps :: String -> Parser Step
    parseAllSteps step = do
      s  <- parseStep step
      ss <- many $ parseStep "And"
      let as = foldr And NoAnd ss
      return $ Step s as

    parseStep :: String -> Parser String
    parseStep step = do
      _ <- string step
      hspace
      str <- many printChar
      space1 <|> eof
      return str

parseLine :: Parser String
parseLine = do
  hspace
  str <- many printChar
  hspace
  _ <- newline
  return str
