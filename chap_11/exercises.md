The | represents logical disjunction – that is, "or."
This is the sum in algebraic datatypes.

nullary constructors are 1
sum types are + or addition when we are talking about cardinality


Exercises: Pity the Bool
1.
```
  data BigSmall =
         Big Bool
       | Small Bool deriving (Eq, Show)

  Big Bool | Small Bool = 2

```

A product type’s cardinality is the product of the cardinalities of its inhabitants

Where a sum type was expressing *or*, a product type expresses *and*

The reason it’s important to understand cardinality is that the cardinality of a datatype roughly equates to how difficult it is to reason about.

Records in Haskell are product types with additional syntax to provide convenient accessors to fields within the record


Product types distribute over sum types
a * (b + c) -> (a * b) + (a * c)

```
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType)

data Author =
    Fiction AuthorName
  | Nonfiction AuthorName deriving (Eq, Show)

```

### Exercises: How Does Your Garden Grow?

1. 
```
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving Show

-- normal form

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving(Show)

```

There are essentially two things we can do with a value. We can generate or construct it or we can match on it and consume it.

Sums are a means of expressing disjunction or the ability to have one of several possible values.

Try to avoid using type synonyms with unstructured data like text or binary. Type synonyms are best used when you want something lighter weight than newtypes but also want your type signatures to be more explicit.

### Exponentiation in what order?

data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)


convert :: Quantum -> Bool

According to the equality of a -> b and b^a there should be 2^3 or 8 implementations of this function.

convert Yes = True
convert No = True
convert Both = True

convert Yes = True
convert No = True
convert Both = False

convert Yes = True
convert No = False
convert Both = False

convert Yes = True
convert No = False
convert Both = True

convert Yes = False
convert No = False
convert Both = False

convert Yes = False
convert No = False
convert Both = True

convert Yes = False
convert No = True
convert Both = True

convert Yes = False
convert No = True
convert Both = False

### The Quad

```
1. data Quad = One
             | Two
             | Three
             | Four
    deriving (Eq, Show)
-- how many different forms can this take?
     eQuad :: Either Quad Quad

A: (4 + 4) = 8

2. prodQuad :: (Quad, Quad)
(4 * 4) = 16

3. funcQuad :: Quad -> Quad
(4^4) = 256

4. prodTBool :: (Bool, Bool, Bool)

(2*2*2) = 8

5. gTwo :: Bool -> Bool -> Bool
(2^2^2 ) = 16

6. fTwo :: Bool -> Quad -> Quad
(2 ^ 4) ^ 4 = 65536

```

Kinds are not types until they are fully applied. Only types have inhabitants at the term level.  
The kind * -> * is waiting for a single * before it is fully applied. The kind * -> * -> * must be applied twice before it will be a real type. This is known as a higher-kinded type. Lists, for example, are higher-kinded datatypes in Haskell.

Getting comfortable with higher-kinded types is important as type arguments provide a generic way to express a “hole” to be filled by consumers of your datatype later

Chapter Exercises
1. Weekday is a type with five data constructors
2. c) f :: Weekday -> String
3. b) must begin with a capital letter
4. c) delivers the final element of xs

### Resources:
https://github.com/mikowitz/haskell-from-first-principles/blob/master/chapter11/Tree.hs
https://github.com/nackjicholson/haskellbook-solutions/blob/master/chapter11/exercises.md 

### Good Functions:
(!!) [1,2,3] 0 -> 1  
zip  
zipWith  
intercalate  
http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List.html#v:intercalate 
