# Banking Server
The server implementing the simple Banking Domain

## TODOs
- restructure modules according to Defect Process structure
- increase typesafety using Phantom Types, GADTs,...
  => MSFs getter can return anything, but we want to restrict it to a specific value
  => implement a proper Money type with currency as phantom type deriving Num and Fractional
- Use haxl for batching db access?
- Implement a 3 layer cake approach
- Implement freer monads approach
- Look into conduit to represent the domain model
- Implement versioning for concurrency control