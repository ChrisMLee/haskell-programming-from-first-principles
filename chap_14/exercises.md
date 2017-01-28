### Chapter 14
You will learn more if you type rather than copy and paste

`do` syntax allows us to sequence monadic actions

QuickCheck relies on the type system to know what kinds of data to generate.  

Property testing is fantastic for ensuring that you’ve met the minimum requirements to satisfy laws, such as the laws of monads or basic associativity. It is not appropriate for all programs, though, as it is not useful for times when there are no assertable, truth-valued properties of the software.

`return` returns a value wrapped inside of a monad

`forAll` uses an explicitly given test case generator.  
`forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property`

Sum types represent disjunction, so with a sum type like Sum, we need to represent the exclusive possibilities in our Gen.

### Good Resources  
[hspec](http://hspec.github.io/)
