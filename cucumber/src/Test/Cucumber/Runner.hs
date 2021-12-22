module Test.Cucumber.Runner where

import           Control.Monad.State.Lazy   (StateT, execStateT)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, parse, some, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, digitChar,
                                             hspace, spaceChar, string)

import           Test.Cucumber.Data.Gherkin (And (..), Given (G), Scenario (..),
                                             Step (..), Then (T), When (W))
import           Test.Cucumber.Data.Step    (StepDefinition (..),
                                             StepParam (Double, Int, Str, Word),
                                             StepType (..))

import           System.IO.Unsafe           (unsafePerformIO)

data StepActionParam
  = ParamWord String
  | ParamString String
  | ParamInt Int
  | ParamDouble Double
  deriving Show

type StepAction s = [StepActionParam] -> StateT s IO ()
type Parser       = Parsec Void String

runScenario :: [(StepType, StepAction s)] -> Scenario -> s -> IO ()
runScenario steps (Scenario _ (G givenStep (W whenStep (T thenStep)))) s = do
    -- putStrLn $ "Given: " ++ show givenStep
    -- putStrLn $ "When: " ++ show whenStep
    -- putStrLn $ "Then: " ++ show thenStep

    -- putStrLn ""
    -- mapM_ (\(t, _) -> print t) steps
    -- putStrLn ""

    givenRet <- execStepActionsFor steps givenStep "Given" s
    case givenRet of
      Nothing -> error "Could not find matching Given steps!"
      (Just givenState) -> do
        whenRet <- execStepActionsFor steps whenStep "When" givenState
        case whenRet of
          Nothing -> error "Could not find matching When steps!"
          (Just whenState) -> do
            thenRet <- execStepActionsFor steps thenStep "Then" whenState
            case thenRet of
              Nothing -> error "Could not find matching Then steps!"
              Just _  -> return ()

execStepActionsFor :: [(StepType, StepAction s)] -> Step -> String -> s -> IO (Maybe s)
execStepActionsFor steps (Step stepStr stepAnd) st s = do
    let stepDefs = map (\(step, act) -> (stepDefinition step, act)) $ filter (\(step, _) -> isStepType st step) steps

    case findFirstParse stepDefs stepStr of
      Nothing  -> do
        print $ "Error: could not find matching action for " ++ st ++ ": " ++ stepStr
        return Nothing
      (Just (params, action)) -> do
        -- print $ "Found matching " ++ st ++ " action for " ++ show stepStr ++ ", parsed params: " ++ show params
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
      -- print $ "Found matching " ++ st ++ " And action for " ++ show andStr ++ ", parsed params: " ++ show params
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
parseStep (Text s cont, act) acc = do
  hspace
  _ <- string s
  hspace
  parseStep (cont, act) acc

parseStep (Param Word cont, act) acc = do
  w <- some alphaNumChar
  parseStep (cont, act) (acc ++ [ParamWord w])

parseStep (Param Str cont, act) acc = do
  _ <- char '\''
  w <- some (alphaNumChar <|> spaceChar <|> char '!' <|> char '.' <|> char '-')
  _ <- char '\''
  parseStep (cont, act) (acc ++ [ParamString w])

parseStep (Param Int cont, act) acc = do
  i <- some digitChar
  parseStep (cont, act) (acc ++ [ParamInt $ read i])

parseStep (Param Double cont, act) acc = do
  p <- some digitChar
  _ <- char '.'
  c <- some digitChar
  parseStep (cont, act) (acc ++ [ParamDouble $ read (p ++ ['.'] ++ c)])

parseStep (StepEnd, act) acc = return (acc, act)
