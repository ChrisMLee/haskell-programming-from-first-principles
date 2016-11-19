module Filtering where
  filterThrees = filter (\x -> (rem x 3) == 0) [1..30]

  getLengthOfThrees = (length . filter (\ x -> (rem x 3) == 0)) [1 .. 30]

  myFilter:: String -> [String]
  myFilter xs = (filter (isNotWords) . words) xs

  isNotWords :: [Char] -> Bool 
  isNotWords "a" = False
  isNotWords "an" = False
  isNotWords "the" = False
  isNotWords _ = True
