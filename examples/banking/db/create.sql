-- DROP DATABASE banking;

-- CREATE DATABASE banking WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.UTF-8';

-- \connect banking;

------------------------------------
-- CUSTOMER 
------------------------------------
DROP TABLE IF EXISTS public.customer;

CREATE TABLE public.customer (
    customer_id SERIAL PRIMARY KEY,
    customer_domain_id VARCHAR(255) NOT NULL,
    customer_name VARCHAR(255) NOT NULL
);
------------------------------------------------------------------------------------------------------------

------------------------------------
-- ACCOUNT 
------------------------------------
DROP TABLE IF EXISTS public.account;

CREATE TABLE public.account (
    account_id SERIAL PRIMARY KEY,
    account_owner_id integer NOT NULL,
    account_iban VARCHAR(255) UNIQUE,
    account_type VARCHAR(255) NOT NULL,

    CONSTRAINT fk_account_owner
      FOREIGN KEY(account_owner_id) 
	      REFERENCES customer(customer_id)
        ON DELETE CASCADE
);

------------------------------------
-- TX Lines 
------------------------------------
DROP TABLE IF EXISTS public.tx;

CREATE TABLE public.tx (
    tx_id SERIAL PRIMARY KEY,
    tx_account integer NOT NULL,
    tx_iban VARCHAR(255) NOT NULL,
    tx_amount real NOT NULL,
    tx_name VARCHAR(255) NOT NULL,
    tx_reference VARCHAR(255) NOT NULL,
    tx_time timestamp NOT NULL,

    CONSTRAINT fk_account
      FOREIGN KEY(tx_account) 
	      REFERENCES account(account_id)
        ON DELETE CASCADE
);

