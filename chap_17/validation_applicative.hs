module ValidationApplicative where

import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation' e a = 
    Failure' e
  | Success' a 
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation' e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' (f a)

-- This is different
instance Monoid e => Applicative (Validation' e) where
  pure = Success'
  (Failure' y) <*> (Failure' x) = Failure' (x <> y)
  (Success' x) <*> (Failure' y) = Failure' y
  (Failure' y) <*> (Success' x) = Failure' y
  (Success' x) <*> (Success' y) = Success' (x y)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Success' a, Failure' b]

instance (Eq a, Eq b) => EqProp (Validation' a b) where (=-=) = eq

main :: IO ()
main = quickBatch $ applicative (undefined :: Validation' String (Int, Double, Char))