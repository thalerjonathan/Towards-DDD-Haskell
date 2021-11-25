module Test.BDD.Cucumber where

import Test.BDD.Runner
import Test.BDD.Data.Gherkin
import Test.BDD.Data.Step

runFeature :: Feature 
           -> ((s -> IO ()) -> IO ())
           -> [(StepType, StepAction s)] 
           -> IO ()
runFeature (Feature _ _ ss) aroundScenario steps = do
  mapM_ (\s -> do 
    aroundScenario (\state -> do
      runScenario steps s state)
    ) ss