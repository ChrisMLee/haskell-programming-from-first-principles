module ConstantInstance where

import Data.Monoid (Monoid, (<>), Sum(Sum, getSum))

newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

-- http://stackoverflow.com/questions/21169943/understanding-data-functor-constant-constructor-and-applicative-laws

instance Monoid m => Applicative (Constant m) where
  -- pure :: a -> Const m a
  -- the 'a' needs to be mempty
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant (x <> y)