Feature: Multiple scenarios

  This feature consists of two scenarios that must succeed regardless
  of being in the same feature.

  However, due to a bug in chuchu (<= 0.1.2), the scenarios succeed
  if executed individually but fail if executed together.

  Scenario: Anybody can use the bathroom
    Given that "Bob" has entered the bathroom and locked the door

  Scenario: Anybody can use the bathroom
    Given that "Alice" has entered the bathroom and locked the door
