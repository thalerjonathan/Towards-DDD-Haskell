module Test.Cucumber
  ( module Test.Cucumber.Data.Gherkin
  , module Test.Cucumber.Data.Step
  , Test.Cucumber.Runner.StepActionParam (..)
  , StepAction
  , given_
  , when_
  , then_

  , runFeature
  , parseFeature
  ) where



import           Data.Void                              (Void)
import           Test.Cucumber.Data.Gherkin             (Feature (..))
import           Test.Cucumber.Data.Step                (StepType)
import           Test.Cucumber.Generator.StepDefinition (given_, then_, when_)
import           Test.Cucumber.Parsing.Gherkin          (parseGherkin)
import           Test.Cucumber.Runner                   (StepAction,
                                                         StepActionParam (..),
                                                         runScenario)
import           Text.Megaparsec                        (ParseErrorBundle,
                                                         parse)

runFeature :: Feature
           -> ((s -> IO ()) -> IO ())
           -> [(StepType, StepAction s)]
           -> IO ()
runFeature (Feature _ _ scenarios) aroundScenario steps = do
  mapM_ (\scenario -> do
    aroundScenario (\s -> do
      runScenario steps scenario s)
    ) scenarios

parseFeature :: String -> Either (ParseErrorBundle String Void) Feature
parseFeature = parse parseGherkin ""
