module DaPhone where

import Data.List
import Data.Char
import Data.Maybe
import Data.Monoid

data DaPhone = DaPhone [Key] deriving (Show) 
  
data Key =  Key {   symbol' :: Symbol' 
                  , values' :: Values'} deriving (Show) 
type Symbol' = Char
type Values' = String

-- getKey :: DaPhone -> Key
-- getKey phone = 
phone :: DaPhone 
phone = DaPhone [   (Key {symbol'='1', values'=""})
                  , (Key {symbol'='2', values'="ABC"})
                  , (Key {symbol'='3', values'="DEF"})
                  , (Key {symbol'='4', values'="GHI"})
                  , (Key {symbol'='5', values'="JKL"})
                  , (Key {symbol'='6', values'="MNO"})
                  , (Key {symbol'='7', values'="PQRS"})
                  , (Key {symbol'='8', values'="TUV"})
                  , (Key {symbol'='9', values'="WXYZ"})
                  , (Key {symbol'='*', values'="^"})
                  , (Key {symbol'='0', values'="+_"})
                  , (Key {symbol'='#', values'=".,"})
                ]

--2.

phoneKeys :: DaPhone -> [Key]
phoneKeys (DaPhone keys) = keys
-- getKey :: DaPhone -> Char -> Key
-- getKey phone 

findBySymbol :: DaPhone -> Char -> Maybe Key
findBySymbol p c = foldr (\x b -> if (symbol' x) == c then (Just x) else b) Nothing (phoneKeys p)

findByChar :: DaPhone -> Char -> Maybe Key
findByChar p c = foldr (\x b -> if (elem (toLower c) $ map toLower $ values' x) then (Just x) else b) Nothing (phoneKeys p)

findKey :: DaPhone -> Char -> Maybe Key
findKey p c =  case findBySymbol p c of
                  Just x -> Just x
                  Nothing -> findByChar p c

-- lower case letters
-- uppercase letters
-- digits 

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int


getPresses :: DaPhone -> Key -> Char -> Presses
getPresses p k c = getPresses (findBySymbol p c) where
                      getPresses (Just x) = (length $ values' k) + 1
                      getPresses Nothing  = (+) 1 $ fromJust $ elemIndex (toLower c) (map toLower $ values' k)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c =  result where
                    key = fromJust $ (findKey p c)
                    digit = symbol' key
                    presses = getPresses p key c
                    result = if (isUpper c) then [('*', 1), (digit, presses)] else [(digit, presses)]
-- fromJust $ (findKey p c)
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

convo :: [String] 
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

convertToKeypresses :: DaPhone -> String -> [[(Digit, Presses)]]
convertToKeypresses p s = map (\x -> if x == ' ' then reverseTaps p '0' else reverseTaps p x) s

tapsToChar :: DaPhone -> [(Digit, Presses)] -> Char
tapsToChar p (x:xs) =  modifier char where
                            operatingSet = if x == ('*', 1) then head xs else x
                            key = fromJust $ findBySymbol p (fst operatingSet)
                            char
                              | snd operatingSet > (length $ values' key) && mod (snd operatingSet) (length $ values' key) == 1 = symbol' key
                              | snd operatingSet <= (length $ values' key) = values' key !! ((snd operatingSet) - 1)
                            modifier = if x == ('*', 1) then toUpper else toLower

convertToPhrase :: DaPhone -> [[(Digit, Presses)]] -> String 
convertToPhrase p xs = map (\x -> tapsToChar p x) xs 

--3. 
fingerTaps :: [(Digit, Presses)] -> Presses 
fingerTaps xs = foldr (\x b -> snd x + b) 0 xs

--4.
mostPopularLetter :: DaPhone -> String -> Char
mostPopularLetter p s = tapsToChar phone (head $ foldr (\x b -> if length x > length b then x else b) [] (group . sort $ convertToKeypresses p s))

--5. 

coolestLtr :: DaPhone -> [String] -> Char
coolestLtr p xs = tapsToChar p $ head $ foldr (\x b -> if length x > length b then x else b) [] $ group . sort $ xs >>= convertToKeypresses p

-- coolestWord :: [String] -> String
-- coolestWord = 





