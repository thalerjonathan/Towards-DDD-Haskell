module Test.Cucumber.Runner where

import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad.State.Lazy

import Test.Cucumber.Data.Gherkin
import Test.Cucumber.Data.Step

import System.IO.Unsafe

data StepActionParam
  = ParamWord String
  | ParamInt Int
  | ParamDouble Double
  deriving Show

type StepAction s = [StepActionParam] -> StateT s IO ()

runScenario :: [(StepType, StepAction s)] -> Scenario -> s -> IO ()
runScenario steps (Scenario _ (G givenStep (W whenStep (T thenStep)))) s = do
    putStrLn $ "Given: " ++ show givenStep
    putStrLn $ "When: " ++ show whenStep
    putStrLn $ "Then: " ++ show thenStep
    
    putStrLn ""
    mapM_ (\(t, _) -> print t) steps
    putStrLn ""

    givenRet <- execStepActionsFor steps givenStep "Given" s
    case givenRet of
      Nothing -> return ()
      (Just givenState) -> do
        whenRet <- execStepActionsFor steps whenStep "When" givenState
        case whenRet of
          Nothing -> return ()
          (Just whenState) -> do
            _thenRet <- execStepActionsFor steps thenStep "Then" whenState
            return ()

execStepActionsFor :: [(StepType, StepAction s)] -> Step -> String -> s -> IO (Maybe s)
execStepActionsFor steps (Step stepStr stepAnd) st s = do
    let stepDefs = map (\(step, act) -> (stepDefinition step, act)) $ filter (\(step, _) -> isStepType st step) steps

    case findFirstParse stepDefs stepStr of
      Nothing  -> do
        print $ "Error: could not find matching action for " ++ st ++ ": " ++ stepStr
        return Nothing
      (Just (params, action)) -> do
        print $ "Found matching " ++ st ++ " action for " ++ show stepStr ++ ", parsed params: " ++ show params
        s' <- execStateT (action params) s
        execStepActionsForAnd stepDefs stepAnd st s'

execStepActionsForAnd :: [(StepDefinition, StepAction s)] -> And -> String -> s -> IO (Maybe s)
execStepActionsForAnd _ NoAnd _ s = return $ Just s
execStepActionsForAnd stepDefs (And andStr andAnd) st s = do
  case findFirstParse stepDefs andStr of
    Nothing -> do
      print $ "Error: could not find matching action for " ++ st ++ " And:" ++ andStr
      return Nothing
    (Just (params, action)) -> do
      print $ "Found matching " ++ st ++ " And action for " ++ show andStr ++ ", parsed params: " ++ show params
      s' <- execStateT (action params) s
      execStepActionsForAnd stepDefs andAnd st s'

findFirstParse :: [(StepDefinition, StepAction s)] -> String -> Maybe ([StepActionParam], StepAction s)
findFirstParse (sd:sds) parseStr = 
    case parse (parseStep sd []) "" parseStr of 
      (Left _err)  -> unsafePerformIO (do
        --putStrLn ""
        --print _err
        return $ findFirstParse sds parseStr)
      (Right p) -> Just p
findFirstParse _ _ = Nothing 

isStepType :: String -> StepType -> Bool
isStepType "Given" (Given _) = True
isStepType "When" (When _)   = True
isStepType "Then" (Then _)   = True
isStepType _ _               = False

stepDefinition :: StepType -> StepDefinition
stepDefinition (Given def) = def
stepDefinition (When def)  = def
stepDefinition (Then def)  = def

parseStep :: (StepDefinition, StepAction s) -> [StepActionParam] -> Parser ([StepActionParam], StepAction s) 
parseStep ((Text s cont), act) acc = do
  hspace
  _ <- string s
  hspace
  parseStep (cont, act) acc

parseStep ((Param Word cont), act) acc = do
  _ <- char '\''
  w <- some (alphaNumChar <|> spaceChar <|> char '!')
  _ <- char '\''
  parseStep (cont, act) (acc ++ [ParamWord w])

parseStep ((Param Int cont), act) acc = do
  i <- some digitChar
  parseStep (cont, act) (acc ++ [ParamInt $ read i])

parseStep ((Param Double cont), act) acc = do
  p <- some digitChar
  _ <- char '.'
  c <- some digitChar
  parseStep (cont, act) (acc ++ [ParamDouble $ read (p ++ ['.'] ++ c)])

parseStep (StepEnd, act) acc = return (acc, act)

type Parser = Parsec Void String