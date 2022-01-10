# Banking Server
The server implementing the simple Banking Domain

## TODOs
- increase typesafety in FreeMSF approach using Phantom Types, GADTs,...
  => MSFs getter can return anything, but we want to restrict it to a specific value
  => implement a proper Money type with currency as phantom type deriving Num and Fractional
- Implement versioning for concurrency control