Feature: Withdrawing money from accounts
  In order to manage my money more efficiently
  As a bank client
  I want to withdraw money from my accounts whenever I need to.

  Scenario: Withdraw money from a Giro account
    Given my Giro account has a balance of 1234.56
    When I withdraw 567.89 from my account
    Then I should have a balance of 666.67 in my account

  Scenario: Withdraw money from a Giro account beyond overdraft limit
    Given my Giro account has a balance of 1000.00
    When I withdraw 2001.0 from my account
    Then I expect the error 'Cannot overdraw Giro account by more than -1000.0!'
    And I should have a balance of 1000.00 in my account

  Scenario: Withdraw money from a Savings account
    Given my Savings account has a balance of 567.89
    When I withdraw 234.56 from my account
    Then I expect the error 'Cannot withdraw money directly from Savings Account! Use transfer of money into a Giro Account of the same customer.'  
    And I should have a balance of 567.89 in my account