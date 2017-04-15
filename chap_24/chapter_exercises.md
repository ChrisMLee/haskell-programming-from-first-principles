# Chapter 24 Exercises  

A parser is a function that takes some textual input (it could be a String in Haskell, or another datatype such as ByteString or Text) and returns some structure as an output.

Combinators are expressions with no free variables  

### Exercises: Parsing Practice
1. There’s a combinator that’ll let us mark that we expect an input stream to be “finished” at a particular point in our parser. In the parsers library this is simply called eof (end-of-file) and is in the Text.Parser.Combinators module. See if you can make the one and oneTwo parsers fail because they didn’t exhaust the input stream!  

`one = char '1' >>= (eof >>) . pure`  
(>>=) :: Monad m => m a -> (a -> m b) -> m b  
eof :: Parsing m => m ()
(>>) :: Monad m => m a -> m b -> m b   
(eof >>) :: (Parsing m, Monad m) => m b -> m b  
(.) :: (b -> c) -> (a -> b) -> a -> c  
pure :: Applicative f => a -> f a  
(eof >>) . pure :: (Parsing m, Monad m) => b -> m b
`oneTwo = char '1' >> char '2' >>= (eof >>) . pure`

### Exercise: Unit of Success  
see [unit_of_success.hs](./unit_of_success.hs

Reference:
[https://crypto.stanford.edu/~blynn/haskell/parse.html](https://crypto.stanford.edu/~blynn/haskell/parse.html)
