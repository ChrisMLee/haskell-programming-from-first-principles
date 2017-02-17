module ChapterExercises where

import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


--1.
data Pair a = 
  Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair c d = Pair (f c) (f d)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

instance Eq a => EqProp (Pair a) where (=-=) = eq


--2. 

data Two a b =
  Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b) 

instance Applicative (Two a) where
  pure _ x = pure 

  -- need to figure out how to write void

main :: IO ()
main = quickBatch $ applicative (undefined :: Pair (Int, Double, Char))