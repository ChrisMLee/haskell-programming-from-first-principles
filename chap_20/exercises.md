# Chapter 20 exercises

Folding necessarily implies a binary associative operation that has an identity value.  

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

`foldMap` first maps each element of the structure to a Monoid and then combines the results using that instance of Monoid.  

`Foldable` is requiring that you make the implicit Monoid visible in folding operations

### Exercises: Library Functions
see [library_functions.hs](./library_functions.hs)

### Links
http://javran.github.io/posts/2014-02-28-type-tetris-and-typeclassopedia.html
