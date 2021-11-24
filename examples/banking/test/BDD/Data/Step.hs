module BDD.Data.Step where

data StepType 
  = Given StepDefinition
  | When StepDefinition
  | Then StepDefinition
  deriving Show

data StepDefinition
  = Text String StepDefinition 
  | Param StepParam StepDefinition 
  | StepEnd
  deriving Show

data StepParam 
  = Word 
  | Int 
  | Double 
  deriving Show