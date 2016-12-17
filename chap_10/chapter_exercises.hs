module ChapTen where
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

--1
stopVowelStopCombos ::[(Char, Char, Char)]
stopVowelStopCombos = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

pStartCombos ::[(Char, Char, Char)]
pStartCombos = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops, s1 == 'p']

nouns, verbs :: [String]
nouns = ["Messi", "Griezmann", "David Luiz"]
verbs = ["tackles", "jukes", "scores"]

nounVerbNounCombos ::[(String, String, String)]
nounVerbNounCombos = [(s1, v, s2) | s1 <- nouns, v <- verbs, s2 <- nouns]

--2
seekritFunc::[Char] -> Int
seekritFunc x =
  div (sum (map length (words x)))
           (length (words x))

--3
-- http://stackoverflow.com/questions/22174035/how-do-i-cast-from-integer-to-fractional
avgWordLength :: (Fractional a) => String -> a
avgWordLength text = numberOfLetters / numberOfWords
  where numberOfWords = (fromIntegral . length . words) text
        numberOfLetters = sum (map (fromIntegral . length) (words text))

-- Rewriting functions using folds
--1
-- FIRST ROUND
--myOr :: [Bool] -> Bool
--myOr [] = False
--myOr (x:xs) = x || myOr xs

-- SECOND ROUND
--myOr :: [Bool] -> Bool
--myOr = foldr
--       (\a b ->
--          if a == True 
--          then True 
--          else b) False

myOr :: [Bool] -> Bool
myOr = foldr (||) False

--2
-- FIRST ROUND
--myAny :: (a -> Bool) -> [a] -> Bool
--myAny f [] = False
--myAny f (x:xs) = f x || myAny f xs

--myAny :: (a -> Bool) -> [a] -> Bool
--myAny f = foldr (\a b -> if f a then True else b) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b ->  f a || b) False

--3 
-- FIRST ROUND
--myElem :: Eq a => a -> [a] -> Bool
--myElem x = any (\a -> a == x)

-- SECOND ROUND
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> a == x || b) False

--4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

--5
--myMap :: (a -> b) -> [a] -> [b]
-- myMap f = foldr (\a b-> (:) (f a) b ) [] 

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) [] 

--6 
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a:b else b) []

--7
-- concat 
squish :: [[a]] -> [a]
squish =  foldr (++) []

--8
-- map then concat
--squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []


--9
-- map identity then concat
squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> x)

--10
-- situation where you need access to the previous 
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy f (x:xs) = foldl compareFn x xs
  where compareFn = (\b a -> if (f a b) == GT then a else b)

--11
-- fold return function sets a or b to its return value depending on how the comparison evaluates
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy f (x:xs) = foldl compareFn x xs
  where compareFn = (\a b -> if (f a b) == LT then a else b)




