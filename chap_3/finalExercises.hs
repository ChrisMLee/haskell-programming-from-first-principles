module FinalExercises where

exclaim :: String -> String
exclaim x = x ++ "!"

takeFourth:: String -> Char
takeFourth x = x !! 4

lastNine:: String -> String
lastNine x = drop 9 x 

thirdChar::String -> Char
thirdChar x = x !! 3

letterIndex :: Int -> Char
letterIndex x =  "Curry is awesome" !! x

rvrs :: String -> String
rvrs x =  concat[ (drop 9 x), " ", (take 2 (drop 6 x)), " ", (take 5 x)]