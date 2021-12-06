# Banking Server
The server implementing the simple Banking Domain

## TODOs
- finish implementation with DDD
  - implement transfer
  - implement add customer / account
- Put interpreters in mock module, bcs its a mock
- refactor Anemic Banking Application service: this doesn't have to be that bad
- implement add to repositories, should return the same thing in the same session without fetching from db? But is this really necessary?
- Try different domain/application layer implementations: tagless final, reader io, freer monads
- Implement collaborative / concurrency control
- Cucumber: make files parsing work and implement all scenario steps