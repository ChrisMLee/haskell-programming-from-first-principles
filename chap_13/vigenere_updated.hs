module VigenereUpdated where
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

main :: IO ()
main = do
  putStr "input vigenere keyword: "
  keyword <- getLine
  putStr "input phrase: "
  phrase <- getLine
  putStrLn $ "encrypted text: " ++ (vigenere keyword phrase)
