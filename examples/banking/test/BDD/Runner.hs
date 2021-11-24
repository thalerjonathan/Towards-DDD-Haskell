module BDD.Runner where

import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import BDD.Data.Gherkin
import BDD.Data.Step

import System.IO.Unsafe

data StepActionParam
  = ParamWord String
  | ParamInt Int
  | ParamDouble Double
  deriving Show

type StepAction = [StepActionParam] -> IO ()

runFeature :: Feature 
           -> IO ()
           -> IO ()
           -> [(StepType, StepAction)] 
           -> IO ()
runFeature f beforeScenario afterScenario steps = do
  -- TODO: run each scenario in separate transaction and roll back at the end to avoid persistent changes to DB. Implement through BeforeScenario

  let (Feature _ _ ss) = f

  mapM_ (\s -> do 
    beforeScenario
    runScenario steps s
    afterScenario) ss

  return ()

runScenario :: [(StepType, StepAction)] -> Scenario -> IO ()
runScenario steps (Scenario _ (G givenStep (W whenStep (T thenStep)))) = do
    putStrLn $ "Given: " ++ show givenStep
    putStrLn $ "When: " ++ show whenStep
    putStrLn $ "Then: " ++ show thenStep
    
    putStrLn ""

    mapM_ (\(t, _) -> print t) steps

    putStrLn ""

    givenRet <- execStepActionsFor steps givenStep "Given"
    if not givenRet 
      then return ()
      else do
        whenRet <- execStepActionsFor steps whenStep "When"
        if not whenRet
          then return ()
          else do
            _thenRet <- execStepActionsFor steps thenStep "Then"
            return ()

    return ()

execStepActionsFor :: [(StepType, StepAction)] -> Step -> String -> IO Bool
execStepActionsFor steps (Step stepStr stepAnd) st = do
    let stepDefs = map (\(s, act) -> (stepDefinition s, act)) $ filter (\(s, _) -> isStepType st s) steps

    case findFirstParse stepDefs stepStr of
      Nothing  -> do
        print $ "Error: could not find matching action for " ++ st ++ ": " ++ stepStr
        return False
      (Just (params, action)) -> do
        print $ "Found matching " ++ st ++ " action for " ++ show stepStr ++ ", parsed params: " ++ show params
        action params
        execStepActionsForAnd stepDefs stepAnd
     
    -- mapM_ (\def -> parseTest (parseStep def []) str) stepDefs
    
  where
    execStepActionsForAnd :: [(StepDefinition, StepAction)] -> And -> IO Bool
    execStepActionsForAnd _ NoAnd = return True
    execStepActionsForAnd stepDefs (And andStr andAnd) = do
      -- mapM_ (\def -> parseTest (parseStep def []) andStr) stepDefs
      case findFirstParse stepDefs andStr of
        Nothing -> do
          print $ "Error: could not find matching action for " ++ st ++ " And:" ++ andStr
          return False
        (Just (params, action)) -> do
          print $ "Found matching " ++ st ++ " And action for " ++ show andStr ++ ", parsed params: " ++ show params
          action params
          execStepActionsForAnd stepDefs andAnd

    findFirstParse :: [(StepDefinition, StepAction)] -> String -> Maybe ([StepActionParam], StepAction)
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

    parseStep :: (StepDefinition, StepAction) -> [StepActionParam] -> Parser ([StepActionParam], StepAction) 
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