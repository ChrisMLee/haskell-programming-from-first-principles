module ChapterExercises where
import Data.Char

-- mapKeyword = (. cycle) . take . length . filter (' ' /=)
mapKeyword::[Char] -> [Char] -> [(Char, Char)]
mapKeyword x y = zip (filter (/=' ') y) (take (length $ filter (/=' ') y) (cycle x))

splitMappedKeyword :: Foldable t => [a] -> t Int -> [[a]]
splitMappedKeyword x y = reverse $ foldr (\a b -> (take a $ drop (sum $ map length b) $ x):b) [] y

-- getShift = subtract (ord 'A') . ord
getShift:: Char -> Int
getShift x = flip (-) (ord 'A') (ord x)

shiftChar:: Int -> Char -> Char
shiftChar shift x =
  case ((+) shift $ ord x) > 90 of
    True -> chr . (+) 64 $ flip mod 90 $ ord x + shift
    False -> chr . (+) shift $ ord x

translateSet:: (Char, Char) -> Char
translateSet (x, y) = shiftChar (getShift y) x

vigenere :: [Char] -> [Char] -> String
vigenere keyword phrase = unwords $ map (map translateSet) (splitMappedKeyword (mapKeyword keyword phrase) (map length (words phrase)))

-- As-patterns

--isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
--isSubsequenceOf [] _ = False
--isSubsequenceOf _ [] = False
--isSubsequenceOf t@(x:xs) z = if elem x z && (length xs == 0) then True else isSubsequenceOf xs z

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = False
isSubsequenceOf _ [] = False
isSubsequenceOf as@(x:xs) bs@(y:ys) = if x == y then isSubsequenceOf xs ys else isSubsequenceOf as ys

--makeWordTuple:: String -> (String, String)
--makeWordTuple xs@(x:ys) = (xs, ((toUpper x):ys) )

--capitalizeWords :: String -> [(String, String)]
--capitalizeWords = map makeWordTuple . words

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map makeWordTuple . words
  where makeWordTuple xs@(x:ys) = (xs, (toUpper x:ys))

-- Language Exercises
capitalizeWord :: String -> String
capitalizeWord (x:xs)= if x /= ' ' then (toUpper x):xs else capitalizeWord xs

--capitalizeParagraph :: String -> String
capitalizeParagraph x = unwords $ map capitalizeWord $ foldr (\a b -> if a == '.' then ".":b else (a:(head b)) : (tail b)) [] x


--foldr (\a b -> if a == '.' then ".":b else (a:(head b)) : (tail b)) [] "blah. woot ha."









