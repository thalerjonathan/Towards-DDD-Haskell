# Cucumber-Haskell

## Other Libraries and Gap
[Chuchu](https://hackage.haskell.org/package/chuchu) is the only library which implements BDD with Cucumber for Haskell using the Gherkin syntax. The idea tho specify the Given/When/Then actions directly as parsers which return values can be directly put into the resutling step action is very nice, however with more than one argument it seems to become quite cumbersome to formulate the parser. Chuchu depends on [Abacate](https://hackage.haskell.org/package/abacate) which is a parser for Gherkin written by the same guys that wrote Chuchu.

So why write a new implementation?
- To give the Haskell community an actively maintained BDD library. Chuchu and Abacate seem not to be actively maintained or updated to newer GHC version and dependencies. The copyright is from 2012 and the last upload for Chuchu was by FelipeLessa at 2014-04-06T14:36:06Z; Abacates was by MarcoSilva at 2012-08-11T12:02:34Z.
- Use of newer GHC version and parsing libraries, built on megaparsec.
- Parser and runner in the same project. While it is a nice feature to separate execution from parsing as Chuchu did with Abacate, I think it overcomplicates things.
- Support for newer Cucumber features / Gherkin keywords.
- More convenient step action defintions through Template Haskell.
- Support for feature files in languages other than english! Chuchu / Abacate supports english only. Step action in code will be english only.

Cucumber-Haskell took .features files from Chuchus test suite as examples because they are simple but good examples and cover a wide range.

## TODOs
- Use Text instead of String
- Use StateT Monad for Step Actions
- Template Haskell based definition of Given/When/Then Step Actions inspired by Servant API definitions
- Gherkin Keywords: Background, Rule, Example, Scenario Outline, Examples. See https://cucumber.io/docs/gherkin/reference/
- Other languages than english