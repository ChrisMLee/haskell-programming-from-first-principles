module WarmingUp where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (,) :: a -> b -> (a, b)

tupled :: [Char] -> ([Char], [Char])
tupled = do
  a <- cap
  b <- rev
  return $ (a, b) 

tupled' :: [Char] -> ([Char], [Char])
tupled' xs = (,) ((return xs) >>= rev) ((return xs) >>= cap) 

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = 
 cap >>=
  \ capped ->
    rev >>=
    \ revved -> return ((,) capped revved)

