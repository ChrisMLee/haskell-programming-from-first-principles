module ZipListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty
  mappend = liftA2 mappend

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 Nil = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a x) = Cons (f a) (fmap f x)

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Cons f y <*> Cons x z = flatMap (\a -> fmap a (Cons x z)) (Cons f y)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs'= let (ZipList' l) = xs
               in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (pure x)
  ZipList' fs <*> ZipList' xs = ZipList' (fmap (\(f, x) -> f x) $ zipList fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = list2List <$> suchThat arbitrary (\x -> length x < 20)

list2List :: [a] -> List a
list2List [] = Nil
list2List (x:xs) = Cons x (list2List xs)

zipList :: List a -> List b -> List (a, b)
zipList Nil _ = Nil
zipList _ Nil = Nil
zipList (Cons x xs) (Cons y ys) = Cons (x, y) (zipList xs ys) 

zipListWith :: (a -> b -> c) -> (List a) -> (List b) -> (List c)
zipListWith _ Nil _ = Nil
zipListWith _ _ Nil = Nil
zipListWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipListWith f as bs)

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

main :: IO ()
main = quickBatch $ applicative ( undefined :: ZipList' (Char, Char, Int) )
