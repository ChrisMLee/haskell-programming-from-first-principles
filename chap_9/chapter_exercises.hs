module ChapNine where
  import Data.Char

  filterUppers:: [Char] -> [Char]
  filterUppers xs = filter isUpper xs

  firstLetterCaps:: [Char] -> [Char]
  firstLetterCaps xs = toUpper (head xs) : tail xs

  capAll:: [Char] -> [Char]
  capAll xs = map toUpper xs

  
  firstAndCap::[Char] -> Char
  firstAndCap = (toUpper . head)

  myOr :: [Bool] -> Bool
  myOr [] = False
  myOr (x:xs) = if x == True then True else myOr xs

  myAny :: (a -> Bool) -> [a] -> Bool
  myAny _ [] = False
  myAny x (y:ys) = if (x y) == True then True else myAny x ys

  myElem :: Eq a => a -> [a] -> Bool
  myElem _ [] = False
  myElem x (y:ys) = if x == y then True else myElem x ys

  myElemTwo :: Eq a => a -> [a] -> Bool
  myElemTwo _ [] = False
  myElemTwo x ys = any ((==)(x)) ys

  myReverse :: [a] -> [a]
  myReverse xs = myReverse xs []  where
    myReverse (x:xs) acc = myReverse xs (x:acc)
    myReverse [] acc = acc

  squish :: [[a]] -> [a]
  squish xs = squish xs [] where
    squish (x:xs) acc = squish xs ((++) acc x)
    squish [] acc = acc

  squishMap :: (a -> [b]) -> [a] -> [b]
  squishMap f xs = squishMap f xs [] where
    squishMap f (x:xs) acc = squishMap f xs ((++) acc (f x))
    squishMap f [] acc = acc

  squishAgain :: [[a]] -> [a]
  squishAgain xs = squishMap (\x -> x) xs

  myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
  myMaximumBy f (x:xs) = myMaximumBy f xs x where
    myMaximumBy f (x:xs) acc = if f x acc == GT then myMaximumBy f xs x else myMaximumBy f xs acc
    myMaximumBy f [] acc = acc

  myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
  myMinimumBy f (x:xs) = myMinimumBy f xs x where
    myMinimumBy f (x:xs) acc = if f x acc == LT then myMinimumBy f xs x else myMinimumBy f xs acc
    myMinimumBy f [] acc = acc


  myMaximum :: (Ord a) => [a] -> a
  myMaximum = myMaximumBy compare

  myMinimum :: (Ord a) => [a] -> a
  myMinimum = myMinimumBy compare


  --myMaximumBy f []

  -- [1, 3, 2]
  --- 1 > 3 ? keep 3 


