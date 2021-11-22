# TODOs

1. Monolith
2. Microservices
3. Exploiting dependent types
4. Implement a Library / Framework

Implement it with BOTH with Java AND Haskell for direct comparison
- Java Monolith
- Haskell Monolith
- Java Microservices using Java Axon
- Haskell Microservices from first principles
- Develop a framework like spring, for monoliths as well as microservices: chimera

We need an interesting and complex domain
- write a product vision of a referee assignment tool which can be used for SE project in WS 2022 (business rules, notifications of fixture changes,...) 
- do Event storming
- do Story mapping
- Define aggregates and contexts
- Draw domain model


Ideas
- what about using Idris 2 and make use of dependent types?
- BDD with haskell: types, quickcheck, dsl,...

What we want
something where haskell code directly reflects the ubiquitous language for which we can also express scenarios directly in code and test it together with quickcheck

What we need
- we need some form of caching (query / results) 
- something like unit of work, where changes to the entities/aggregates are tracked and then flushed to the DB at the end of the TX.
- type safe REST api using servant
- some kind of service bus to help us with asynchronous processing of domain events for eventual consistency
- versioning of aggregates for optimistic/pessimistic concurrency control in collaborative domains

Research Questions
- Can we exploit stm to enforce tx boundaries of aggregates?
- How can we implement it as Microservices?
- How can we represent stateful aggregates, reacting to different "commands"? Aggregate: has different functions which are called depending on what happens: arrival of a command, arrival of a domain event. therefore with these "callbacks" we can implement a pure aggregate
- can we achieve typing across microservices with the type safe REST API Definition of servant?
- parse feature at compile time to make sure that the arguments are correct?

Hypotheses
- Haskell has to be the perfect fit for DDD: strong static types and free monads for a ubiquitous language and modeling, with purity to keep aggregates free from technology and implement them as stateful but pure computations. 
- Free monads=ubiquitous language
- Purity = aggregates, entities, value objects => Domain Logic is pure computation!
- Lazy evaluation = tracking changes to aggregates, flushed at the end of the TX
- Stm = composable tx boundaries

Related Research
- Domain Driven Design Made Dependently Typed by Andor Penzes

Relevant Books / References
- FaDD Granin
- Haskell in Depth
- Domain Modeling Made Functional Wlaschin
- DDD evans
- DDD Vernon
- DDD Millett
- Strategic Monoliths
