module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
-- http://hackage.haskell.org/package/dlist import qualified Data.DList as DL
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

-- main :: IO()
-- main = 
--   mapM_ (putStrLn . fizzBuzz) [1..100]

-- fizzbuzzList :: [Integer] -> [String]
-- fizzbuzzList list = 
--   let dlist = execState (mapM_ addResult list) DL.empty
--   in DL.apply dlist [] -- convert back to normal list

fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list = 
  execState (mapM_ addResult list) DL.empty

-- addResult :: Integer -> State (DL.DList String)
addResult n = do
    xs <- get
    let result = fizzBuzz n
    -- snoc appends to the end, unlike 
    -- cons which adds to the front
    put (DL.snoc xs result)

main :: IO()
main = 
  mapM_ putStrLn $ fizzbuzzList [1..100]

