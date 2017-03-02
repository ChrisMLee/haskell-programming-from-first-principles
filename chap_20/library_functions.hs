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
elem' a ta = foldr (\x acc -> if x == a then True else False) False ta

--4.
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' ta = foldr z Nothing ta
             where
              z x Nothing = Just x
              z x (Just y) = if x < y then (Just x) else (Just y)