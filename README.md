# phoenix
A Haskell framework inspired by Java Spring Boot for writing Monoliths and Microserivce backends with DB mapping and Unit of Work, Caching, Mail, Messaging, REST, DDD, TDD, BDD, CQRS/ES support out of the box.

## Features

### DB Mapping and Unit of Work
TODO

### Caching
TODO

### Mail
TODO

### Messaging
TODO

### Logging
TODO

### REST
TODO

### DDD
TODO

### Eventual Consistency
TODO

### TDD
TODO

### BDD
TODO

### CQRS/ES
TODO

## TODOs
- experiment with Domain Layer

- work on cucmber-haskell
  - parse given/when/then_ TH

- start working on the phoenix framework
  - Use TH to parse rest/html endpoints into servant types like spring, but directly in types e.g. handleDepositAccount :: SqlBackend -> [getREST|/account/deposit?iban:text&amount:double|]