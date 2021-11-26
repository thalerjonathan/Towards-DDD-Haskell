module Test.Cucumber where

import Test.Cucumber.Runner
import Test.Cucumber.Data.Gherkin
import Test.Cucumber.Data.Step

runFeature :: Feature 
           -> ((s -> IO ()) -> IO ())
           -> [(StepType, StepAction s)] 
           -> IO ()
runFeature (Feature _ _ scenarios) aroundScenario steps = do
  mapM_ (\scenario -> do 
    aroundScenario (\s -> do
      runScenario steps scenario s)
    ) scenarios