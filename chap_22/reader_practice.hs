module ReaderPractice where

import Control.Applicative
import Data.Maybe
import Data.Monoid

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> zs <*> ys

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (,) (z' n) (z' n)  

summed :: Num c => (c,c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = do
  one <- (>3)
  two <- (<8)
  return $ one && two

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO()
main = do
  print $ getAll $ foldMap All (sequA 3)
  print $ fromMaybe [] $ sequA <$> s'
  print $ fromMaybe True $ bolt <$> ys

-- print $ sequenceA [Just 3, Just 2, Just 1]
-- print $ sequenceA [x, y]
-- print $ sequenceA [xs, ys]
-- print $ summed <$> ((,) <$> xs <*> ys)
-- print $ fmap summed ((,) <$> xs <*> zs)
-- print $ bolt 7
-- print $ fmap bolt z
-- print $ sequenceA [(>3), (<8), even] 7









