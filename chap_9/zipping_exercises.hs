module Zip where
  myZip :: [a] -> [b] -> [(a, b)]
  myZip _ [] = []
  myZip [] _ = []
  myZip a b = (head a, head b) : myZip (tail a) (tail b)


--  map :: (a -> b) -> [a] -> [b]
--  map _ [] = []
---- [1] [2] [3]
--map f(x:xs) = f x: map f xs