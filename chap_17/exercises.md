# Chapter 17 Exercises

__Monoid__ gives us a means of mashing two values of the same type together.  
__Functor__, on the other hand, is for function application over some structure we don’t want to have to think about.

__Applicative__, is a _monoidal functor_ where the function we’re applying is also embedded in some structure. Because the function and the value it’s being applied to both have structure, we have to smash those structures together. Every type that can have an Applicative instance must also have a Functor instance. With Applicative, we have a Monoid for our structure and function application for our values

When you use `Applicative` the function you want to map is inside the same structure as the value you want to apply it to e.g.  
`Just (Person (Name "babe")) <*> Just (Address "farm")`

`Applicative`: we fmap'd my function over some functorial `f`  
or it already was in `f` somehow. I want to do something kinda like an fmap, but my function is embedded in the functorial structure too, not just the value I want to apply my function to

`(<*>) :: Applicative f => f (a -> b) -> f a -> f b`

To the left of `<*>` must always be a function embedded in some structure.

```
(+) <$> (Just 9001) <*> (Just 6)
f (a -> b)  
Just (+) 9001 _ <*> (Just 6)  
Just (+) 9001 6  
Just 9007  
```
`liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c`  
```
liftA2 (+) (Just 9001) (Just 6)
Just 9007
```

`pure` has type `Applicative f => a -> f a`, we can think of it as a means of embedding a value of any type in the structure we’re working with  
`fmap f x = pure f <*> x`  

The “structure” that pure is providing there isn’t really meaningful.  

instance - type
(under `where`) - data constructor

### Resources
[http://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr](http://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr)


### 17.5 Exercises: Lookups
see [lookups.hs](./lookups.hs)

### 17.5 Exercise: Identity Instance
see [identity_instance.hs](./identity_instance.hs)

### 17.5 Exercise: Constant Instance
see [constant_instance.hs](./constant_instance.hs)

### 17.5 Exercise: Fixer Upper  
1. `const <$> Just "Hello" <*> Just "World"`  
2. `(,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]`  

### 17.8 List Applicative Exercise
see [list_applicative.hs](./list_applicative.hs)

### 17.8 ZipList Applicative Exercise
see [ziplist_applicative.hs](./ziplist_applicative.hs)

### 17.8 Validation Applicative Exercise
see [validation_applicative.hs](./validation_applicative.hs)  

### 17.9 Chapter Exercises  
1.  
-- Type []  
-- Methods    
`pure ::a -> [a]`  
`(<*>) :: [(a -> b)] -> [a] -> [b]` [[+1]] [1]

2.  
-- Type IO  
-- Methods  
`(pure :: a -> IO a) "sdf"`
`((<*>) :: IO (a -> b) -> IO a -> IO b)` (return (+1)) (return 1)

3.  
-- Type (,) a  
-- Methods  
`(pure :: Monoid b => a -> ((,) b) a) 4 :: (String, Int)`  
`((<*>) :: Monoid c =>  ((,) c (a -> b)) -> ((,) c a) -> ((,) c b)) ("sdf", (+4)) ("sdf", 7)`

4.  
-- Type (->) e  
-- Methods  
`(pure :: Monoid e => a -> (->) e a) "hello" "world"`
`(<*>) ::  Monoid e => ((->) e) (a -> b) -> ((->) e) a -> ((->) e) b`
