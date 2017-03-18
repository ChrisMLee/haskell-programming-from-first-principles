module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.Monoid

--1.
newtype Identity' a = 
  Identity' a deriving (Eq, Ord, Show)

instance Functor Identity' where
  fmap f (Identity' a) = Identity' (f a)

instance Foldable Identity' where
   foldr f z (Identity' a) = f a z 

instance Traversable Identity' where
  traverse f (Identity' a) = Identity' <$> f a

instance (Arbitrary a) => Arbitrary (Identity' a) where
  arbitrary = do
    a <- arbitrary
    return (Identity' a)


instance (Eq a) => EqProp (Identity' a) where (=-=) = eq

--2. 

newtype Constant' a b =
  Constant' { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant' a) where
  fmap f (Constant' a) = Constant' a 

instance Foldable (Constant' a) where
  foldr f z (Constant' a) = z 

instance Traversable (Constant' a) where
  traverse f (Constant' a) = pure (Constant' a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant' a b) where
  arbitrary = do
    a <- arbitrary
    return (Constant' a)

instance (Eq a, Eq b) => EqProp (Constant' a b) where (=-=) = eq

--3. 

data Optional a =
    Nada 
  | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap f Nada = Nada
  fmap f (Yep a) = Yep (f a) 

instance Foldable Optional where
  foldr f z Nada = z
  foldr f z (Yep a) = f a z 

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a


instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof [ return $ Yep a
          , return $ Nada
          ]

instance (Eq a) => EqProp (Optional a) where (=-=) = eq


--4.

data List' a =
    Nil 
  | Cons a (List' a) deriving (Eq, Ord, Show)

instance Functor List' where
  fmap f Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Foldable List' where
  foldMap _ Nil = mempty
  foldMap f (Cons x y) = (f x) <> foldMap f y

-- List' (f b) becomes f (List' b) with sequenceA
instance Traversable List' where
  traverse f Nil = pure Nil
  traverse f (Cons x y) = Cons <$> f x <*> (sequenceA $ fmap f y)

instance (Arbitrary a) => Arbitrary (List' a) where
  arbitrary = list2List <$> suchThat arbitrary (\x -> length x < 20)

list2List :: [a] -> List' a
list2List [] = Nil
list2List (x:xs) = Cons x (list2List xs)

instance (Eq a) => EqProp (List' a) where (=-=) = eq

--quickBatch $ traversable (undefined :: List' (Int, Int, [Int]))

--5. 

data Three a b c =
  Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c) 

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)


instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

--6. 
data Three' a b =
  Three' a b b deriving (Eq, Ord, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Foldable (Three' a) where
  foldMap f (Three' w x y) = (f x) <> (f y)

instance Traversable (Three' a) where
  traverse f (Three' x y z) = (Three' x) <$> (f y) <*> (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

--7. 

data S n a = 
  S (n a) a deriving (Eq, Ord, Show)

instance Functor n => Functor (S n) where
  fmap f (S x a) = S (fmap f x) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S x a) =  foldMap f x <> (f a)

instance Traversable n => Traversable (S n) where
  traverse f (S x a) = S <$> (traverse f x) <*> (f a)

-- (traverse f (n a)) -> f (n b). S <$> f (n b). f( S (n b) ) <*> (f a). f (S (n b) b )

--traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

--8.
data Tree a = 
   Empty
 | Leaf a
 | Node (Tree a) a (Tree a)
 deriving (Eq, Show)


instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node xa a ya) = Node (fmap f xa) (f a) (fmap f ya)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node xa a ya) = (foldMap f xa) <> (f a) <> (foldMap f ya)

  foldr f z Empty = z
  foldr f z (Leaf a) = f a z
--foldr :: Foldable t => (a -> b -> b) -> b -> Tree a -> b
--foldr f z (Node xa a ya) = foldr f (foldr f (f a z) xa) ya
  foldr f z (Node l x r) =
      let base = f x z --b
          left = foldr f base l
          right = foldr f left r
      in right

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> (f a)
  traverse f (Node xa a ya) =  Node <$> (traverse f xa) <*> f a <*> (traverse f ya)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    x <- arbitrary
    l <- arbitrary
    r <- arbitrary
    frequency [(1, return Empty),
               (1, return (Leaf x)),
               (1, return (Node l x r))]

instance Eq a => EqProp (Tree a) where (=-=) = eq

main = do
  quickBatch $ traversable (undefined :: Identity' (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Constant' Int (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: List' (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Three Int Int (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Three' Int (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))


