module Blah where

f :: Int -> String
f x
  | x `mod` 15 == 0 = "FizzBuzz"
  | x `mod` 3  == 0 = "Fizz"
  | x `mod` 5  == 0 = "Buzz"
  | otherwise       = ""

g x = let y = f x
        in y
  where f = undefined -- blahghghghghhg
