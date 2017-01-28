# Chapter 15 exercises

_algebra_ -  abstract patterns in code which have well-defined, lawful representations in mathematics. One or more operations and the set they operate over. Algebras are defined by their laws and are useful principally for their laws. Laws make up what algebras are.

_set_ - type

A monoid is a function that takes two arguments and follows two laws: _associativity_ and _identity_.

Associativity means the arguments can be regrouped (or reparenthesized, or reassociated) in different orders and give the same result, as in addition.  

Identity means there exists some value such that when we pass it as input to our function, the operation is rendered moot and the other value is returned, such as when we add zero or multiply by one. Monoid is the typeclass that generalizes these laws across types. There’ll be some value which, when combined with any other value, will always return that other value

Example:
List
Identity - [1,2,3] (++) [] = [1,2,3]
Associative - ([1,2,3] (++) [1] (++)) [3,3] = [1,2,3] (++) ([1] (++) [3,3])

https://wiki.haskell.org/State_Monad?

**The typeclass abstracts and generalizes the pattern so that you write code in terms of any type that can be monoidally combined**

In Haskell, we think of types as having an instance of a typeclass. When we represent abstract operations that can be reused across a set of types, we usually represent them as a typeclass.

The typeclass Monoid is defined:
```
class Monoid m where
mempty :: m
mappend :: m -> m -> m
mconcat :: [m] -> m
mconcat = foldr mappend mempty
```

Proofs are programs, and programs are proofs. We care about programs that compose well, that are easy to understand, and which have predictable behavior. To that end, we should steal prolifically from mathematics. If we want to be able to stack up functions scalably, they need to obey laws.

```

Prelude> foldr mappend mempty ([2, 4, 6] :: [Product Int])
Product {getProduct = 48}

Prelude> foldr mappend mempty ([2, 4, 6] :: [Sum Int])
Sum {getSum = 12}

Prelude> foldr mappend mempty ["blah", "woot"]
"blahwoot"
```

When we have more than one potential implementation for Monoid for a datatype, it’s most convenient to use newtypes to tell them apart, as we did with Sum and Product.

`<>` - mappend

 `mappend`-ing is less about combining and more about condensing or reducing

 mempty is used to present identity monoid values, such as "" and Sum 0 and empty bytestrings. **Whenever we use mappend between mempty and some other monoid value, the result is that other monoid value.**

 Commutative means you can reorder the arguments and still get the same result.

 Associativity simply says that you can associate the arguments of your operation differently and the result will be the same

An orphan instance is when an instance is defined for a datatype and typeclass, but not in the same module as either the declaration of the typeclass or the datatype. If you don’t “own” the typeclass or the datatype, newtype it!

 Writing orphan instances should be avoided at all costs. Do not be lazy about this! If you get an orphan instance warning from GHC, fix it. The solution: put type and typeclass in the same file. If you don't own either the type or typeclass, create a newtype wrapping the original type and define instances on that type.

You want to be careful to assert types so that QuickCheck knows which Arbitrary instance to get random values for testing from. You can use`verboseCheck` to see what values were tested


Check the `mempty` result for a type:
```
Prelude> mempty :: String
""
```

### Good Functions
fst :: (a, b) -> a  
snd :: (a, b) -> b  

### Unsolved Problems
First.hs
15.14, #9 - Combine
15.14, #10 - Comp - How to quickcheck this?

### Exercises

#### Optional Monoid

```
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (mappend x y)
  mappend Nada (Only x) = (Only x)
  mappend (Only x) Nada = (Only x)
```
#### Maybe Another Monoid
see [maybe_another_monoid.hs](./maybe_another_monoid.hs)
