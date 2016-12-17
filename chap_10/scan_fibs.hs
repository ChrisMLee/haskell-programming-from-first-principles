module ScanFibs where
  fibs = 1 : scanl (+) 1 fibs
  
  --fibsHun = if 

  --fibsHun:: [Integer]
  --fibsHun = fibsHun [1] where
  --  fibsHun acc = if head acc > 100 then reverse $ tail acc else fibsHun ((sum . take 2 $ acc): acc)

  fibsHun::[Integer]
  fibsHun = takeWhile (< 100) fibs

  addFibs::(Num a) => [a] -> a
  addFibs = sum . take 2


  -- Scans exercises
  firstTwentyFibs = take 20 $ 1 : scanl (+) 1 fibs
 
  factorial:: (Num a, Enum a) => a -> [a]
  factorial n = take 20 $ scanl (*) 1 [1..n]

  --if x > 100 -> xs
  --else 

  --fibsUoH = foldr (\a b -> :()) [] [1..10] 

  --foldr (\x xxs-> (xxs) [] [1..10]

  
  --acc 

  --myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
  --myMinimumBy f (x:xs) = myMinimumBy f xs x where
  --  myMinimumBy f (x:xs) acc = if f x acc == LT then myMinimumBy f xs x else myMinimumBy f xs acc
  --  myMinimumBy f [] acc = acc

  --1 : scanl (+) 1 fibs
  -- [x | x <- fibs, x < 100]

  --fibsUnderOneHundred = foldr (\a b -> if a < 100 then a:b else b) [] fibs