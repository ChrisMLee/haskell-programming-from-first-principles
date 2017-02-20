module EitherMonad where
import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum' a b =
    First' a
  | Second' b
  deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap f (First' a) = First' a
  fmap f (Second' b) = Second' (f b)

instance (Monoid a) => Applicative (Sum' a) where
  pure = Second'
  (First' a) <*> _ = First' a
  _ <*> (First' a) = First' a
  (Second' f) <*> (Second' b) = Second' (f b)

instance Monoid a => Monad (Sum' a) where
  return = pure
  (>>=) (First' a) f = First' a
  (>>=) (Second' b) f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum' a b) where
  arbitrary = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First' a,
         return $ Second' b]

-- (>>=) :: Either e a -> (a -> Either e b) -> Either e b

instance (Eq a, Eq b) => EqProp (Sum' a b) where (=-=) = eq

main :: IO()
main = do
  quickBatch $ applicative (undefined :: Sum' String (Int, Double, Char))
  quickBatch $ monad (undefined :: Sum' String (Int, Double, Char))