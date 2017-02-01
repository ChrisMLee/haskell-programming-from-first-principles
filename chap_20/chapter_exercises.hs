module ChapterExercises where

import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum
--4.
maximum' = foldr (max . Just) Nothing

newtype Max x = Max {getMax :: Maybe a} deriving (Eq, Ord, Show)

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  (Max Nothing) `mappend` y = y
  x `mappend` (Max Nothing) = x
  (Max x) `mappend` (Max y) = Max $ max x y

maximum = getMax .foldMap (Max . Just)

minimum = foldr (\l r -> Just $ maybe l id (min (Just l) r)) Nothing

--8.
foldr (:) []
