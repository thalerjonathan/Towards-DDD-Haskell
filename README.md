# Towards Tactical Domain-Driven Design in Haskell

## Introduction

A very viable and popular approach to architect Haskell applications is the so called [3 Layere Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html), a term coined by Matt Parsons to describe this architectural style. When analysing this from my Software Engineering (SE) background, I relate Parsons 3 Layer Cake to the following SE terminology:

- **Layer 1 is the Application Layer**. This layer controls the flow of Use Cases / User Stories / Scenarios. It therefore is the client to Layer 2 and 3, that is, it uses the Infrastructure (Layer 2) to call into the Domain Layer (Layer 3) to execute domain (business) logic and react to results / exceptions happening in the domain logic. Another fundamentally important responsibility of the Application Layer is transaction management. This means the layer clearly defines the transaction boundaries, which generally encompass the flow of the whole Use Case - what we clearly don't want is each DB access running in its own transaction, but that all changes made in the Use Case either transact or fail atomically. The Application Layer is also responsible for security-related concerns such as enforcing access rules by making sure that certain functionality is only accessible by certain roles or users. Generally, this layer is called by a 4th layer, generally called View (aka Presentation) Layer, which can be quite generic, for example server-side rendering controllers, REST handlers, messaging handlers, or a simple CLI from the console.

- **Layer 2 is the Infrastructure Layer**. This layer holds and implements all access to external services, which are used by Layer 1 (Application Layer) to interact with the outside world and manage cross-cutting concerns. Generally this layer contains: caching, logging, e-mail, web servers for HTTP requests, database access, messaging. Note that although the 4th Layer is responsible for calling into the Application Layer (Layer 1), it fundamentally relies on infrastructure to enable it. This means although from an architectural standpoint HTTP handlers are implemented in the 4th Viewing Layer, they are ultimately called from the backing infrastructure. Therefore I distinguish between infrastructure the application builds on / uses behind the scenes and the Infrastructure Layer which the Application Layer (Layer 1) uses explicitly.

- **Layer 3 is the Domain Layer**. This layer is the very heart of the application and basically encapsulates the specific domain knowledge of the business for which the application was written for. Its very essence is that it should be as pure as possible, ideally consisting only of pure computation being completely independent from outside, infrastructural influences and dependencies. This makes it possible to express the domain itself with its business rules as directly as possible in code.

We can therefore conclude that the very essence of this architecture is ultimately to enable and *put a purely computational Domain Layer into the heart of the application*. This Domain Layer is where your business shines, it encapsulates the business knowledge of your company, has been developed, refined and learned over a longer period of time and is ultimately the know-how where your business excells in and therefore your most valuable asset. It therefore **is the domain expressed in code, reflecting the business language.** This is known in Domain-Driven Design (DDD) as the *Ubiquitous Language*. Ultimately the aim of DDD is to distill the essence of the core domain of a business and express it with its language as directly in purely computational code as possible.

DDD was conceived by Eric Evans in his seminal book TOOD by putting forward a number of patterns which deal with Strategic and Tactical Design. Strategic Design deals with how to distill various concepts in a business and draw boundaries to distill domains and subdomains and the core domain of the business. Tactical Design then deals with how to actually derive software designs which reflects the Strategic Design, that is the (core) domain(s) as directly as possible in code. The equally seminal book by Vaughn Vernon (Implementing Domain-Driven Design) shows how to implement DDDs Tactical Design patterns in Code using Java. 

DDD as conceived by Evans and as implemented by Vernon (and others, such as Millett and Nilsson in C#) can be said to be applied primarily in OOP and has not found wide use outside of OOP. **To my best knowledge DDD as conceived by Evans (and implemented by Vernon) has not been applied to Haskell.** This text, the accompanying *Banking Example* and the resulting *Phoenix Library* is an attempt to explore and bring the use of DDD, more specifically the Tactical Patterns as conceived by Evans and implemented by Vernon, to Haskell. 

We can therefore formulate the following research questions:
- Which Tactical DDD Patterns are valuable to bring to Haskell?
- What are the benefits of bringing the Tactical DDD Patterns to Haskell?
- How can we bring the selected Tactical Patterns to Haskell?

From this we derive a few initial hypotheses to start with and which form our initial research assumptions and direction:
- The valuable Tactical Patterns are: Value Objects, Entities, Aggregates, Repositories, and Domain Events.
- The benefits are
  - Value Objects: they are essentially normal data types because each data type in Haskell is immutable. By proper module design hiding internals of Value Objects we can represent them using ADTs we can enforce the immuability
  - Entities and Aggregates: we need to represent things in a domain which have an immutable identity but other parts which can change over time. Haskell is not very strong in these terms, and it is there where OOP really shines. Also, for enforcing strong (transactional) and eventual business rules and consistencies together with transactional boundaries should be much easier if we can come up with a concept for Entities and Aggregates.
  - Repositories: a domain-specific way to get instances of Aggregates without actually directly involving low-level DB queries in the Domain Layer / Application Layer. Should mimic a collection but maps to the DB behind the scenes. It is there where we ideally implement some form of *Unit of Work* which tracks all changes to Entities/Aggregates to make DB access much more transparent and simpler.
  - Domain Events: a very functional approach of communicating changes in a Domain, which fits naturally to functional programming in Haskell. They should be highly valuable to allow an Aggregate/Domain Model communicate necessary side effects outside of the pure Domain Model. The Domain Events should then be processed and delegated by the Application Layer.
- The most challenging part is implementing Aggregates due to their immutable identity but mutable properties for which consistency and business rules need to be enforced.

## Backgroud
In this background section we briefly introduce various concepts necessary to fully understand the implementation sections, as well as discussing related work to put ours into context.

### Domain-Driven Design
In this section we cover and describe the Tactical DDD Patterns very briefly. An in-depth discussion of all Tactical Patterns in particular and DDD in general is beyond the scope of this text and we refer to Evans and Vernons excellent Books.

- Value Object
- Entity
- Aggregate
- Repositories
- Domain Event
- Domain Service
- Factories

### Related Work
There exists the book *Domain Modeling Made Functional: Tackle Software Complexity with Domain-Driven Design and F#* by Scott Wlashin, which investigates DDD in the context of F#. 

A master thesis has been written at the University of Applied Sciences Vorarlberg on *Applying Domain-Driven Design in Elixir*. 

Andor Pénzes has done work on dependently typed DDD based on the Book by Scot Wlashin. His code is publicly available as a [GitHub Repo](https://github.com/andorp/order-taking/)

