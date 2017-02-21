# Chapter 18 exercises

`concat :: [[a]] -> [a]`

Monad, in a sense, is a generalization of concat  
`join :: Monad m => m (m a) -> m a`  

The argument the monad TestBatch wants is identical to the Applicative, a tuple of three value types embedded in the structural type.

Monad is an applicative that can bind

Applicative makes no guarantee of order.  
Things are guaranteed to happen in order in a Monad.  

You should only use Monad if you have to - ytry to use the weakest possible constraint you can.
Otherwise you pigeonhole yourself into doing things sequential

IO, errors, data structures

### The answer is the exercise
```
bind :: Monad m => (a -> m b) -> m a -> m
bind = bind f a = join $ fmap f a
```

### Short Exercise: Either Monad
see [either_monad.hs](./either_monad.hs)
