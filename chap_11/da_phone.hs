module DaPhone where

import Data.List
import Data.Char
import Data.Maybe

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

phoneKeys :: DaPhone -> [Key]
phoneKeys (DaPhone keys) = keys
-- getKey :: DaPhone -> Char -> Key
-- getKey phone 

findBySymbol :: DaPhone -> Char -> Maybe Key
findBySymbol p s = foldr (\x b -> if (symbol' x) == s then (Just x) else b) Nothing (phoneKeys p)

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
                      getPresses (Just x) = 0
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


-- "ABC2"




-- type Action = { type: "REQUEST" } 
--             | { type: "SUCCESS", data: string } 
--             | { type: "FAILURE", error: any }