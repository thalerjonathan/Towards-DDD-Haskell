Feature: Depositing money into accounts
  In order to manage my money more efficiently
  As a bank client
  I want to deposit money into my accounts whenever I need to.

  Scenario: Deposit money into a Giro account
    Given my Giro account has a balance of 1234.56
    When I deposit 567.89 into my account
    Then I should have a balance of 1802.45 in my account

  Scenario: Deposit money into a Savings account
    Given my Savings account has a balance of 567.89
    When I deposit 234.56 into my account
    Then I expect the error 'Cannot deposit money directly into Savings Account! Use transfer of money from a Giro Account of the same customer.'  
    And I should have a balance of 567.89 in my account