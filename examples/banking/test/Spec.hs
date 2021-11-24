-- {-# LANGUAGE OverloadedStrings          #-}
module Main where

-- import Text.Megaparsec
-- import Data.Text
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

main :: IO ()
main = do
  let _scenario1 = "Scenario: Deposit money into a Giro account\n" ++
                   "Given my Giro account has a balance of 1234.56\n" ++
                   "When I deposit 567.89 into my account\n" ++ 
                   "Then I should have a balance of 1802.45 in my account\n"

  let _scenario2 = "Feature: Transfering money between accounts\n" ++
                   "In order to manage my money more efficiently\n" ++
                   "As a bank client\n" ++
                   "I want to transfer money between accounts whenever I need to.\n" ++
                  "Scenario: Transfer money from a Savings account to another Giro account of different customers\n" ++ 
                  "Given a Savings account with Iban 'AT12 12345 01234567890' and a balance of 1000.0\n" ++
                  "And a Giro account with Iban 'AT98 98765 09876543210' and a balance of 500.0\n" ++
                  "When Transferring 200.0 from Iban 'AT12 12345 01234567890' to Iban 'AT98 98765 09876543210'\n" ++
                  "Then I expect the error 'Cannot transfer from Savings account!'\n" ++
                  "And There should be a balance of 1000.0 in the account with Iban 'AT12 12345 01234567890'\n" ++
                  "And There should be a balance of 500.0 in the account with Iban 'AT98 98765 09876543210'"

  --s <- readFile "test/resources/features/Depositing.feature"
  --print s
  parseTest parseFeature _scenario2

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