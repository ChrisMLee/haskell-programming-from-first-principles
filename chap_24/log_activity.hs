{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}

module LogActivity where

import Text.RawString.QQ
import Text.Trifecta
import Data.List
import Control.Applicative
import Data.Time
import Data.Monoid

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

newtype Comment = Comment String deriving (Eq, Show)
-- Todo: change to a type with a month, date, year
newtype Date = Date String deriving (Eq, Show)

data Line = 
    LineDate Date
  | LineComment Comment
  | LineActivity (Activity String)
  deriving (Eq, Show)

base10Conv :: [Int] -> Int
base10Conv = foldl' (\b a -> a + (b * 10)) 0

parseActivity :: Parser (Activity String)
parseActivity = do
  h <- count 2 digit
  skipMany (oneOf ":")
  m <- count 2 digit
  t <- space *> manyTill anyChar (try (string "\n") <|> (string "--"))
  return (Activity (Hours h) (Minutes m) (Title t))

parseComment :: Parser Comment
parseComment = Comment <$> (string "--" *> space *> manyTill anyChar (try (string "\n")))

parseDate :: Parser Date
parseDate = Date <$> (char '#' *> space *> manyTill anyChar (try (string "\n")))

-- letterDashSpace :: Parser Char
-- letterDashSpace = try letter <|> char '-' <|> char ' ' <|> char '&'
-- try parseActivity, try parseComment

parseWord :: Parser [[Char]]
parseWord = try (some (token $ some letter)) <|> return []

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

-- commment, date, line
parseLine :: Parser Line
parseLine = try (LineDate <$> parseDate) <|> (LineComment <$> parseComment) <|> (LineActivity <$> parseActivity)

parseLogFile :: Parser [Line]
parseLogFile = do
  skipWhitespace
  lines <- some parseLine
  return lines

-- Activity (Hours "08") (Minutes "00") (Title "Breakfast"),Activity (Hours "09") (Minutes "00") (Title "Sanitizing moisture collector")
-- calculateActivityTime :: (Activity String) -> (Activity String) -> String
ja = (Activity (Hours "08") (Minutes "00") (Title "Breakfast"))
rule = (Activity (Hours "12") (Minutes "00") (Title "Lunch"))


calculateActivityTime (Activity h m t) (Activity h' m' t') = 
  whoo h m h' m' where
    whoo (Hours x) (Minutes y) (Hours x') (Minutes y') =
      ( (abs $ floor (diffUTCTime (understandTime (x <> ":" <> y)) (understandTime (x' <> ":" <> y')))  `div` 60 `div` 60),
       floor (diffUTCTime (understandTime (x <> ":" <> y)) (understandTime (x' <> ":" <> y')))  `div` 60 `rem` 60)

--- 

timeFormat :: String
timeFormat = "%H:%M"
understandTime = parseTimeOrError True defaultTimeLocale timeFormat

timeUno :: UTCTime
timeUno = understandTime "10:30"

timeDos :: UTCTime
timeDos = understandTime "12:37"

-- noice :: (Activity String) -> String
noice (Activity x y z) = doope x where
                          doope (Hours x') = x'

-- calculateActivityTimes :: [Line] -> [String]
calculateActivityTimes list = foldr timeTillNext [] list where
                                timeTillNext (LineActivity x) b = x : b
                                timeTillNext (LineComment x) b  = b
                                timeTillNext (LineDate x) b     = b

-- diffUTCTime
-- DiffTime
-- floor (diffUTCTime timeDos timeUno)
-- floor (diffUTCTime timeDos timeUno) `div` 60 `rem` 60
-- weirdThing = foldr cool [] [1,2,3] 
--               where cool a b = (a + 1) : b

-- elemIndex 

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

sectionTwo :: String
sectionTwo = [r|-- wheee a comment
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

-- [08:00, 09:00, 11:00, 12:00, 13:00, 17:00, 17:30, 19:00, 21:00, 21:15, 22:00]

-- list, fold list + position 
-- Calculate activity times
--  

main :: IO()
main = do
  -- print $ parseString parseActivity mempty "08:00 Breakfast"
  -- print $ parseString parseActivity mempty "09:00 Sanitizing moisture collector"
  -- print $ parseString (some parseLine) mempty sectionOne 
  print $ parseString parseLogFile mempty logFile
  -- print $ parseString parseComment mempty "-- wheee a comment\n"
  -- print $ parseString parseDate mempty "# 2025-02-05\n"
  -- print $ parseString parseLine mempty "08:00 Breakfast\n"

-- reduce
-- LineActivity x
-- b 



-- [LogSection, LogSection]
-- LineActivity (Activity (Hours "19") (Minutes "00") (Title "Dinner"))
-- LineActivity (Activity (Hours "21") (Minutes "00") (Title "Shower"))
-- Activity1 (function) Activity2 = [Title, Duration], [Title, Hours]




