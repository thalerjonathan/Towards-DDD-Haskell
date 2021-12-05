DROP SCHEMA IF EXISTS banking CASCADE;
CREATE SCHEMA IF NOT EXISTS banking;

------------------------------------
-- CUSTOMER 
------------------------------------
DROP TABLE IF EXISTS banking.customer;

CREATE TABLE banking.customer (
    customer_id SERIAL PRIMARY KEY,
    customer_domain_id VARCHAR(255) NOT NULL,
    customer_name VARCHAR(255) NOT NULL
);
------------------------------------------------------------------------------------------------------------

------------------------------------
-- ACCOUNT 
------------------------------------
DROP TABLE IF EXISTS banking.account;

CREATE TABLE banking.account (
    account_id SERIAL PRIMARY KEY,
    account_owner_domain_id VARCHAR(255) NOT NULL,
    account_balance NUMERIC(32, 2) NOT NULL,
    account_iban VARCHAR(255) UNIQUE,
    account_type VARCHAR(255) NOT NULL

    -- CONSTRAINT fk_account_owner
    --   FOREIGN KEY(account_owner_id) 
	  --     REFERENCES banking.customer(customer_id)
    --     ON DELETE CASCADE
);
 
------------------------------------
-- TX Lines 
------------------------------------
DROP TABLE IF EXISTS banking.tx;

CREATE TABLE banking.tx (
    tx_id SERIAL PRIMARY KEY,
    tx_account INTEGER NOT NULL,
    tx_iban VARCHAR(255) NOT NULL,
    tx_amount NUMERIC(32, 2) NOT NULL,
    tx_name VARCHAR(255) NOT NULL,
    tx_reference VARCHAR(255) NOT NULL,
    tx_time TIMESTAMP NOT NULL,

    CONSTRAINT fk_account
      FOREIGN KEY(tx_account) 
	      REFERENCES banking.account(account_id)
        ON DELETE CASCADE
);

------------------------------------
-- TX Lines 
------------------------------------
DROP TABLE IF EXISTS banking.event;

CREATE TABLE banking.event (
  event_id SERIAL PRIMARY KEY,
  event_created TIMESTAMP NOT NULL,
  event_type VARCHAR(255) NOT NULL,
  event_processed BOOLEAN NOT NULL,
  event_failed BOOLEAN NOT NULL,
  event_failed_error VARCHAR(255) NOT NULL,
  event_payload VARCHAR(4096) NOT NULL
);

---------------------------------------------------------------------------------------
-- Jonathan Thaler Customer
---------------------------------------------------------------------------------------
INSERT INTO banking.customer 
  (customer_domain_id, customer_name)
VALUES
  ('cad6e64d-23a2-4598-8a77-17d6b8e3733b', 'Jonathan Thaler')
RETURNING customer_id;



-- GIRO ACCOUNT
INSERT INTO banking.account
  (account_iban, account_balance, account_type, account_owner_domain_id)
VALUES
  ('AT12 12345 01234567890', 1300, 'Giro', 'cad6e64d-23a2-4598-8a77-17d6b8e3733b');

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (1, 'AT98 98765 09876543210', 1000, 'Deposit', 'Deposit', NOW());

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (1, 'AT98 98765 09876543210', 100, 'Max Mustermann', 'Rent', NOW());

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (1, 'AT98 98765 09876543210', 200, 'Max Mustermann', 'Rent', NOW());

-- SAVINGS ACCOUNT
INSERT INTO banking.account
  (account_iban, account_balance, account_type, account_owner_domain_id)
VALUES
  ('AT23 45678 01234567890', 2500, 'Savings', 'cad6e64d-23a2-4598-8a77-17d6b8e3733b');

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (2, 'AT98 98765 09876543210', 2000, 'Deposit', 'Deposit', NOW());

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (2, 'AT98 98765 09876543210', 300, 'Max Mustermann', 'Rent', NOW());

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (2, 'AT98 98765 09876543210', 200, 'Max Mustermann', 'Rent', NOW());
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
-- Thomas Schwarz Customer
---------------------------------------------------------------------------------------
INSERT INTO banking.customer 
  (customer_domain_id, customer_name)
VALUES
  ('c06f7abf-3735-46cf-9088-a1515f58a79f', 'Thomas Schwarz');

-- GIRO ACCOUNT
INSERT INTO banking.account
  (account_iban, account_balance, account_type, account_owner_domain_id)
VALUES
  ('AT34 56789 01234567890', 500, 'Giro', 'c06f7abf-3735-46cf-9088-a1515f58a79f');

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (3, 'AT98 98765 09876543210', 150, 'Deposit', 'Deposit', NOW());

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (3, 'AT98 98765 09876543210', 200, 'Max Mustermann', 'Rent', NOW());

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (3, 'AT98 98765 09876543210', 250, 'Max Mustermann', 'Rent', NOW());

-- SAVINGS ACCOUNT
INSERT INTO banking.account
  (account_iban, account_balance, account_type, account_owner_domain_id)
VALUES
  ('AT45 67890 01234567890', 1500, 'Savings', 'c06f7abf-3735-46cf-9088-a1515f58a79f');

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (4, 'AT98 98765 09876543210', 1000, 'Deposit', 'Deposit', NOW());

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (4, 'AT98 98765 09876543210', 200, 'Max Mustermann', 'Rent', NOW());

INSERT INTO banking.tx
  (tx_account, tx_iban, tx_amount, tx_name, tx_reference, tx_time)
VALUES
  (4, 'AT98 98765 09876543210', 300, 'Max Mustermann', 'Rent', NOW());
---------------------------------------------------------------------------------------