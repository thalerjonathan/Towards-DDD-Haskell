# Banking System
The goal is to implement a simple banking system for bank employees where they can view customers, their accounts and transactions, as well as performing the following operations:

1. Depositing money into an account.
2. Withdrawing money from an account.
3. Transferring money between different accounts.

## Domain Description and Business Rules

- Customers have a name.
- Customers can have multiple accounts.
- Each account is owned by a specific customer.
- An account has a unique IBAN.
- There are Giro and Savings accounts.
- A Giro account can be overdrawn by 1000€.
- Withdrawing and depositing money is only possible from/into a Giro account.
- Withdrawing and depositing money from/into Savings accounts can only happen through a transfer from/into a Giro account owned by the same Customer.
- A Savings account can not be overdrawn.
- After withdrawing or depositing money, the account should immediately reflect the new balance.
- Transferring money can happen only between Giro accounts and only up to a limit of 5000€ per transaction. The exception is a transfer between a Giro and Savings account of the same customer.
- When transferring money between accounts, the money is immediately withdrawn from the sending account, therefore immediately reflected in the balance.
- Transferring money between accounts always takes some time and does not show up immediately in the receiving account.
- When transferring fails the money is eventually transferred back to the sending account after some time.

## TODOs
- Optimistic concurrency control with versioning of Aggregates for safe collaboration
