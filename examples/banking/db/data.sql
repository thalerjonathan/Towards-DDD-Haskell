---------------------------------------------------------------------------------------
-- Jonathan Thaler Customer
---------------------------------------------------------------------------------------
INSERT INTO customer 
  (customer_domain_id, customer_name)
VALUES
  ('cad6e64d-23a2-4598-8a77-17d6b8e3733b', 'Jonathan Thaler')
RETURNING customer_id;

-- GIRO ACCOUNT
INSERT INTO account
  (account_iban, account_type, account_owner_id)
VALUES
  ('AT12 12345 01234567890', 'GIRO', 2);

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (2, 'AT98 98765 09876543210', 1234, 'Deposit', 'Deposit', NOW());

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (2, 'AT98 98765 09876543210', 123, 'Max Mustermann', 'Rent', NOW());

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (2, 'AT98 98765 09876543210', 123, 'Max Mustermann', 'Rent', NOW());

-- SAVINGS ACCOUNT
INSERT INTO account
  (account_iban, account_type, account_owner_id)
VALUES
  ('AT23 45678 01234567890', 'SAVINGS', 2);

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (3, 'AT98 98765 09876543210', 2345, 'Deposit', 'Deposit', NOW());

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (3, 'AT98 98765 09876543210', 345, 'Max Mustermann', 'Rent', NOW());

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (3, 'AT98 98765 09876543210', 345, 'Max Mustermann', 'Rent', NOW());
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
-- Thomas Schwarz Customer
---------------------------------------------------------------------------------------
INSERT INTO customer 
  (customer_domain_id, customer_name)
VALUES
  ('cad6e64d-23a2-4598-8a77-17d6b8e3733b', 'Thomas Schwarz');

-- GIRO ACCOUNT
INSERT INTO account
  (account_iban, account_type, account_owner_id)
VALUES
  ('AT34 56789 01234567890', 'GIRO', 3);

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (6, 'AT98 98765 09876543210', 1234, 'Deposit', 'Deposit', NOW());

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (6, 'AT98 98765 09876543210', 123, 'Max Mustermann', 'Rent', NOW());

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (6, 'AT98 98765 09876543210', 123, 'Max Mustermann', 'Rent', NOW());

-- SAVINGS ACCOUNT
INSERT INTO account
  (account_iban, account_type, account_owner_id)
VALUES
  ('AT45 67890 01234567890', 'SAVINGS', 3);

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (8, 'AT98 98765 09876543210', 2345, 'Deposit', 'Deposit', NOW());

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (8, 'AT98 98765 09876543210', 345, 'Max Mustermann', 'Rent', NOW());

INSERT INTO tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (8, 'AT98 98765 09876543210', 345, 'Max Mustermann', 'Rent', NOW());
---------------------------------------------------------------------------------------