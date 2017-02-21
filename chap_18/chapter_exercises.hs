module ChapterExercises where
import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad


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

--3. 

newtype Identity a = 
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a) 

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
  a <- arbitrary
  return $ (Identity a)

--4. 

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)


instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a x) = Cons (f a) (fmap f x)

instance Applicative List where
  pure x = Cons x Nil
  Cons f y <*> Cons x z = flatMap (\a -> fmap a (Cons x z)) (Cons f y)
  _ <*> Nil = Nil
  Nil <*> _ = Nil

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Monad List where
  return = pure
  (>>=) (Nil) f = Nil
  (>>=) (Cons x y) f = (f x) `append` (y >>= f)
  -- y :: List a
  -- f x :: List b
  -- y  :: List b
  -- (a -> List b)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = list2List <$> suchThat arbitrary (\x -> length x < 20)

list2List :: [a] -> List a
list2List [] = Nil
list2List (x:xs) = Cons x (list2List xs)

instance (Eq a) => EqProp (List a) where (=-=) = eq

main :: IO()
main = do
  quickBatch $ applicative (undefined :: Nope (Int, Double, Char))
  quickBatch $ monad (undefined :: Nope (Int, Double, Char))
  quickBatch $ applicative (undefined :: PhhhbbtttEither String (Int, Double, Char))
  quickBatch $ monad (undefined :: PhhhbbtttEither String (Int, Double, Char))
  quickBatch $ applicative (undefined :: Identity (Int, Double, Char))
  quickBatch $ monad (undefined :: Identity (Int, Double, Char))
  quickBatch $ applicative (undefined :: List (Int, Double, Char))
  quickBatch $ monad (undefined :: List (Int, Double, Char))



-- Write the following functions using the methods provided by Monad and Functor.
-- Using stuff like identity and composition is fine, but it has to typecheck with types provided.

--1.
j :: Monad m => m (m a) -> m a
j = join

--2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

--3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

--4. 
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

--5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f =  (:) <$> f x <*> (meh xs f)

-- meh = forM 

--6. 



