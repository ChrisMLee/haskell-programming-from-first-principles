module Correcting where

x = (+)

f:: [a] -> Int
f xs = 
  let w = length xs
  in x 1 w
      