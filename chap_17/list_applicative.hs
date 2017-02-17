module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Applicative List where
  pure x = Cons x Nil
  Cons f y <*> Cons x z = flatMap (\a -> fmap a (Cons x z)) (Cons f y)
  _ <*> Nil = Nil
  Nil <*> _ = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a x) = Cons (f a) (fmap f x)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = list2List <$> suchThat arbitrary (\x -> length x < 20)

list2List :: [a] -> List a
list2List [] = Nil
list2List (x:xs) = Cons x (list2List xs)

instance (Eq a) => EqProp (List a) where (=-=) = eq

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

-- needs a function that works like List (a -> b) for `f`


main :: IO ()
main = quickBatch $ applicative $  (undefined :: List (Int, Double, Char))









