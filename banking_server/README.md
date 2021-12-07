# Banking Server
The server implementing the simple Banking Domain

## TODOs
- refactoring
  - Anemic Banking Application service
    - restructure domain rules into functions
    - exception handling

- testing
  - implement easy way to write composable mocks (with Free Monads should be easy, accept incomplete patterns)
  - test application layer
  - test domain layer
  
  - Cucumber: make files parsing work and implement all scenario steps
  - Cucumber: run against all Application Layers

- experiment on Domain Models
  - tagless final
  - reader io
  - freer monads

- Implement collaborative / concurrency control