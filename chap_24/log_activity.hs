{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}

module LogActivity where

import Text.RawString.QQ
import Text.Trifecta
import Data.List
import Control.Applicative

-- http://ekmett.github.io/trifecta/

-- TotalLog data type made up of different activities
-- parseActivity: monadic action that grabs the name of the activty, its time
-- and the time that it took until the next activity
-- use mod for minutes out of hour
-- tuple (8, 45, title)

-- skip comments
-- skip date

newtype Hours a = Hours a deriving (Eq, Show, Functor)
newtype Minutes a = Minutes a deriving (Eq, Show, Functor)
newtype Title a = Title a deriving (Eq, Show, Functor)

data Activity a =
  Activity (Hours a) (Minutes a) (Title a) 
  deriving (Eq, Show, Functor)

base10Conv :: [Int] -> Int
base10Conv = foldl' (\b a -> a + (b * 10)) 0

parseActivity :: Parser (Activity String)
parseActivity = do
  skipWhitespace
  h <- count 2 digit
  skipMany (oneOf ":")
  m <- count 2 digit
  t <- space *> some letterDashSpace
  return (Activity (Hours h) (Minutes m) (Title t))

letterDashSpace :: Parser Char
letterDashSpace = try letter <|> char '-' <|> char ' '

parseWord :: Parser [[Char]]
parseWord = try (some (token $ some letter)) <|> return []

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

sectionOne :: String
sectionOne = [r|
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
|]


logFile :: String
logFile = [r| 
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep


# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

main :: IO()
main = do
  print $ parseString parseActivity mempty "08:00 Breakfast"
  print $ parseString parseActivity mempty "09:00 Sanitizing moisture collector"
  print $ parseString (some parseActivity) mempty sectionOne 









