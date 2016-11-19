module Arith3Broken where

main :: IO ()
main = do print (1 + 2)
          putStrLn "ten"
          print (negate (-1))
          print ((+) 0 (negate 1))