Feature: Transfering money between accounts
  In order to manage my money more efficiently
  As a bank client
  I want to transfer money between accounts whenever I need to.

Scenario: Transfer money from a Giro account to another Giro account of different customers
Given a Giro account with Iban 'AT55 12345 01234567890' and a balance of 1000.0
And a Giro account with Iban 'AT98 98765 09876543210' and a balance of 500.0
When Transferring 200.0 from Iban 'AT55 12345 01234567890' to Iban 'AT98 98765 09876543210'
Then There should be a balance of 800.0 in the account with Iban 'AT55 12345 01234567890'
And There should be a balance of 700.0 in the account with Iban 'AT98 98765 09876543210'

Scenario: Transfer money from a Giro account to another Giro account of different customers beyond the transfer limit
Given a Giro account with Iban 'AT55 12345 01234567890' and a balance of 1000.0
And a Giro account with Iban 'AT98 98765 09876543210' and a balance of 500.0
When Transferring 5001.0 from Iban 'AT55 12345 01234567890' to Iban 'AT98 98765 09876543210'
Then I expect the error 'Transfer between different customers cannot exceed 5000!'  
And There should be a balance of 1000.0 in the account with Iban 'AT55 12345 01234567890'
And There should be a balance of 500.0 in the account with Iban 'AT98 98765 09876543210'

Scenario: Transfer money from a Savings account to another Giro account of different customers
Given a Savings account with Iban 'AT55 12345 01234567890' and a balance of 1000.0
And a Giro account with Iban 'AT98 98765 09876543210' and a balance of 500.0
When Transferring 200.0 from Iban 'AT55 12345 01234567890' to Iban 'AT98 98765 09876543210'
Then I expect the error 'Transfer cannot happen with Savings account of different customers!'  
And There should be a balance of 1000.0 in the account with Iban 'AT55 12345 01234567890'
And There should be a balance of 500.0 in the account with Iban 'AT98 98765 09876543210'

Scenario: Transfer money from a Giro account to a Savings account of the same customer
Given a Giro account with Iban 'AT55 12345 01234567890' and a balance of 1000.0 
And a Savings account of the same customer with Iban 'AT98 98765 09876543210' and a balance of 500.0
When Transferring 200.0 from Iban 'AT55 12345 01234567890' to Iban 'AT98 98765 09876543210'
Then There should be a balance of 800.0 in the account with Iban 'AT55 12345 01234567890'
And There should be a balance of 700.0 in the account with Iban 'AT98 98765 09876543210'

Scenario: Transfer money from a Giro account to a Giro account of the same customer exceeding 5000
Given a Giro account with Iban 'AT55 12345 01234567890' and a balance of 10000.0 
And a Giro account of the same customer with Iban 'AT98 98765 09876543210' and a balance of 500.0
When Transferring 6000.0 from Iban 'AT55 12345 01234567890' to Iban 'AT98 98765 09876543210'
Then There should be a balance of 4000.0 in the account with Iban 'AT55 12345 01234567890'
And There should be a balance of 6500.0 in the account with Iban 'AT98 98765 09876543210'