module ChapterExercises where
import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- 1.
data Nope a = 
  NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
   fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure x = NopeDotJpg
  (<*>) NopeDotJpg NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) NopeDotJpg f = NopeDotJpg

instance (Arbitrary a) => Arbitrary (Nope a) where
  arbitrary = do
  return $ NopeDotJpg

instance (Eq a) => EqProp (Nope a) where (=-=) = eq

--2.

data PhhhbbtttEither b a = 
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap f (Right' b) = Right' b 

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  _ <*> (Right' b) = Right' b
  (Right' b) <*> _ = Right' b
  (Left' f) <*> (Left' b) = Left' (f b)

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Left' a) f = f a 
  (>>=) (Right' b) f = Right' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
  a <- arbitrary
  b <- arbitrary
  oneof [ return $ Left' a, 
          return $ Right' b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where (=-=) = eq

main :: IO()
main = do
  quickBatch $ applicative (undefined :: Nope (Int, Double, Char))
  quickBatch $ monad (undefined :: Nope (Int, Double, Char))
  quickBatch $ applicative (undefined :: PhhhbbtttEither String (Int, Double, Char))
  quickBatch $ monad (undefined :: PhhhbbtttEither String (Int, Double, Char))









