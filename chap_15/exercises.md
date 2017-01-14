# Chapter 15 exercises

A monoid is a function that takes two arguments and follows two laws: associativity and identity.

Associativity means the arguments can be regrouped (or reparenthesized, or reassociated) in different orders and give the same result, as in addition.  

Identity means there exists some value such that when we pass it as input to our function, the operation is rendered moot and the other value is returned, such as when we add zero or multiply by one. Monoid is the typeclass that generalizes these laws across types.

Example:
List 
Identity - [1,2,3] (++) [] = [1,2,3]
Associative - ([1,2,3] (++) [1] (++)) [3,3] = [1,2,3] (++) ([1] (++) [3,3])

https://wiki.haskell.org/State_Monad?