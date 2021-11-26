module Test.Cucumber.Data.Gherkin where

data Feature
  = Feature String String [Scenario]
  deriving Show

data Scenario 
  = Scenario String Given
  deriving Show

data Given = G Step When
  deriving Show

data When = W Step Then
  deriving Show

data Then = T Step
  deriving Show

data Step = Step String And
  deriving Show

data And 
  = And String And 
  | NoAnd
  deriving Show