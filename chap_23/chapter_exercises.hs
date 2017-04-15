{-# LANGUAGE InstanceSigs #-}

module ChapterExercises where

import Data.Monoid
import Control.Monad

newtype Moi s a = 
  Moi { runMoi :: s -> (a,s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> (f $ fst (g(s)), s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a,s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> ((fst $ (f(s))) $ (fst $ (g(s))), s)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = join $ Moi $ \s -> (g $ (fst $ (f(s))), s)

--1. 

get :: Moi s s
get = Moi $ \s -> (s, s)

--2. 

put :: s -> Moi s ()
put a = Moi $ \s -> ((), a) 

--3.

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ (sa(s))

--4. 

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ (sa(s))

--5. 

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f(s))

-- :t runMoi (modify (+1) >> modify (+1)) 0
-- types work

