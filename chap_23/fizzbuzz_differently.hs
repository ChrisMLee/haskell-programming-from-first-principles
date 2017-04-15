module FizzBuzzDifferently where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

-- addResult :: Integer -> State [String]
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo x y = foldr (\a z -> (fizzBuzz a):z) [] [x..y]

--  execState (mapM_ addResult [1..10]) []

-- foldr (\a z -> x:z) [] [1..10]

main :: IO()
main = 
  mapM_ putStrLn $ fizzbuzzFromTo 1 100