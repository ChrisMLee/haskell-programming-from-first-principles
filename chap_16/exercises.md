# Chapter 16 Exercises

Functor is a way of lifting over structure (mapping) in such a manner that you don’t have to care about the structure because you’re not allowed to touch the structure anyway.  

A Functor is a way to apply a function over or around some structure that we don't want to alter.

The point of Functor is to reify and be able to talk about cases where we want to reuse functions in the presence of more structure and be transparently oblivious to that additional structure.

Functors obey two basic laws:  
Identity `fmap id == id`  
Composition  `fmap (f.g) == fmap f . fmap g`


_Composition:_  
This law says composing two functions lifted separately should produce the same result as if we composed the functions ahead of time and then lifted the composed function all together. Maintaining this property is about preserving composability of our code and preventing our software from doing unpleasantly surprising things.

Forming hypotheses, creating experiments based on them or modifying existing experiments, and validating them is a critical part of becoming comfortable with abstractions like Functor!


fmap is function composition

Review: You define an instance of arbitary for the `type` and return a `data constructor`

instance - type
(under `where`) - data constructor

Functor instances can only be created for types of kind `* -> *`
. Anything larger becomes part of the functorial structure and cannot be mapped over.

Functions are functors!
given:
d = ((return '1' ++) . show)(\x -> [x, 1..3])  
d = fmap ((return '1' ++) . show)(\x -> [x, 1..3])

Functor instances for Maybe and Either are handy for times you intend to ignore the left cases, which are typically your error or failure cases. Because fmap doesn’t touch those cases, you can map your function right to the values that you intend to work with and ignore those failure cases.

### Questions
1. Q: How do you write an arbitrary for (16.17 #5, #6 - last section):  

```
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)
```

2. q: 16.17 #11 - Is this correct?

```
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print a b) = Print a (f b)
  fmap f (Read g) = Read (fmap f g)
```


### 16.4 Exercises: Be Kind
1. What’s the kind of a?  
`a -> a`  
answer: *  

2. What are the kinds of b and T ? (The T is capitalized on purpose!)  
`a -> b a -> T (b a)`  
answer:  
b = `* -> *`  
T = `* -> *`

3. What’s the kind of c?  
c a b -> c b a  
answer: `* -> * -> *`

### 16.7 Exercises: Heavy Lifting
see [heavy_lifting.hs](./heavy_lifting.hs)

### 16.10 Exercises: Instances of Func
see [instances_of_func.hs](./instances_of_func.hs)

### Exercise: Possibly
see [possibly.hs](./possibly.hs)

### 16.17 Chapter exercises
Determine if a valid Functor can be written for the datatype provided.

1.  `data Bool =  
      False | True`  
No, its of kind `*`.

2. `data BoolAndSomethingElse a =
False' a | True' a`  
Yes. kind is `* -> *`

3. `data BoolAndMaybeSomethingElse a =
Falsish | Truish a`  
Yes

4. `newtype Mu f = InF { outF :: f (Mu f) }`  
Yes  

5. `data D =
D (Array Word Word) Int Int`  
No.

see [rearrange.hs](./rearrange.hs)  
see [chapter_exercises.hs](./chapter_exercises.hs)
