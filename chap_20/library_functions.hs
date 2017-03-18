module LibaryFunctions where

import Data.Monoid
import Data.Maybe

--1. 

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

--2. 

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

--3. 

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = foldr (\x acc -> if x == a then True else False) False

--4.
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr z Nothing
             where
              z x Nothing = Just x
              z x (Just y) = if x < y then (Just x) else (Just y)

--5.
--null' :: (Foldable t) => t a -> Bool
--null' = foldr z False
--          where
--            z (s x) acc = if x then True else False

--6. 
length' :: (Foldable t) => t a -> Int
length' = foldr z 0
            where 
              z x acc = acc + 1

--7.
toList' :: (Foldable t) => t a -> [a]  
toList' = foldr z []
            where
              z x acc = acc ++ [x]

--8.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (\x -> x <> mempty)

--9. 
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f ta = foldr (\x acc -> (f x) <> acc ) mempty ta














