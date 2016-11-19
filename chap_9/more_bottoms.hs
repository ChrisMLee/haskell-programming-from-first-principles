module MoreBottoms where

  boolThree =  map (\x -> bool 3 (-3) $ (x == 3)) [1..10]
--map (\x -> if x == 3 then (-x) else (x)) [1..10]