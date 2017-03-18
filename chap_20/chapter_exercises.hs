module ChapterExercises where

import Data.Monoid
import Data.Foldable
import Data.Bool

--1.
data Constant a b =
  Constant a

instance Foldable (Constant a) where
  foldr f z (Constant x) = z
  foldMap f (Constant x) = mempty

--2. 
data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f z (Two a b) = f b z
  foldMap f (Two a b) = f b

--3.
data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z
  foldMap f (Three a b c) = f c 

--4. 
data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = (f b) <> (f c)

-- print $ foldMap Sum (Three' "a" 1 2)

--5. 
data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = (f b) <> (f c) <> (f d)

-- print $ foldMap Sum (Four' "a" 1 2 3)

--6. 

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if (f x) then pure x else mempty)

--filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
--filterF f = foldMap (\x -> bool)

































--sum' :: (Foldable t, Num a) => t a -> a
--sum' = getSum . foldMap Sum
----4.
--maximum' = foldr (max . Just) Nothing

--newtype Max x = Max {getMax :: Maybe a} deriving (Eq, Ord, Show)

--instance Ord a => Monoid (Max a) where
--  mempty = Max Nothing
--  (Max Nothing) `mappend` y = y
--  x `mappend` (Max Nothing) = x
--  (Max x) `mappend` (Max y) = Max $ max x y

--maximum = getMax .foldMap (Max . Just)

--minimum = foldr (\l r -> Just $ maybe l id (min (Just l) r)) Nothing

----8.
--foldr (:) []
