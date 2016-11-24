module Zip where
  myZip :: [a] -> [b] -> [(a, b)]
  myZip _ [] = []
  myZip [] _ = []
  myZip x y = (head x, head y) : myZip (tail x) (tail y)

  myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  myZipWith f _ [] = []
  myZipWith f [] _ = []
  myZipWith f x y = (f (head x) (head y)) : myZipWith f (tail x) (tail y)

  rewiredZip x y = myZipWith (,) x y

--  map :: (a -> b) -> [a] -> [b]
--  map _ [] = []
---- [1] [2] [3]
--map f(x:xs) = f x: map f xs