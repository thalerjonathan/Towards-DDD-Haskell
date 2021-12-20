Feature: Chuchu should know how to catch failure

  Scenario: Unknown step definition must stop execution
    Given non-existent step

  Scenario: Step that fails must stop execution
    Given failing step
