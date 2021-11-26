# Banking System With Domain Model
This implementation now brings in a proper Domain Model, which is mapped to the DB behind the scenes using Free Monads

- Do a naive approach directly in the rest handlers with IO, without layers, enforcing all business rules in the rest handler

- use Church Encoded Free monads for performance
- Do a tagless final approach
- Do a freer monads approach
