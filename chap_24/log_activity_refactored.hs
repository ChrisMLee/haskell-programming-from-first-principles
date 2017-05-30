{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuasiQuotes #-}

module LogActivity where

import Control.Applicative 
import Data.Char
import Data.List
import Data.Semigroup (Semigroup, (<>))
import Data.Time
import Text.RawString.QQ
import Text.Trifecta

newtype Hours a
  = Hours
    { getHours :: a
    } deriving (Eq, Show, Functor)

newtype Minutes a
  = Minutes
    { getMinutes :: a
    } deriving (Eq, Show, Functor)

newtype Title a
  = Title
  { getTitle :: a
  } deriving (Eq, Show, Functor)

data Activity a = Activity
  (Hours a) (Minutes a) (Title a)
  deriving (Eq, Show, Functor)

newtype Comment = Comment String deriving (Eq, Show)

newtype Date = Date String deriving (Eq, Show)

data Line =
    LineDate Date
  | LineComment Comment
  | LineActivity
    {
    getActivity :: (Activity String)
    }
  deriving (Eq, Show)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (fmap . fmap)

base10Conv :: [Int] -> Int
base10Conv = foldl' (\b a -> a + (b * 10)) 0

parseActivity :: Parser (Activity String)
parseActivity = do
  h <- count 2 digit
  skipMany (oneOf ":")
  m <- count 2 digit
  t <- space *> manyTill anyChar (
    try (string "\n")
    <|> (string " --" *> space *> manyTill anyChar (try (string "\n"))))

  pure (Activity (Hours h) (Minutes m) (Title t))

parseComment :: Parser Comment
parseComment = Comment <$> (string "--" *> space *> manyTill anyChar (try (string "\n")))

parseDate :: Parser Date
parseDate = Date <$> (char '#' *> space *> manyTill anyChar (try (string "\n")))

parseLine :: Parser Line
parseLine =
  try (LineDate     <$> parseDate)
  <|> (LineComment  <$> parseComment)
  <|> (LineActivity <$> parseActivity)

parseSection = do
  skipMany parseComment
  date <- LineDate <$> parseDate
  lines <- some parseLine
  return (date : lines)


parseLogFile :: Parser [[Line]]
parseLogFile = do
  skipWhitespace
  sections <- some (token parseSection)
  return sections

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

-- via http://stackoverflow.com/a/39818693
timeFormat :: String
timeFormat = "%H:%M"
understandTime = parseTimeOrError True defaultTimeLocale timeFormat

-- via http://stackoverflow.com/questions/376968/convert-haskell-int-with-leading-zero-to-string/376982#376982
show2d :: Int -> String 
show2d n | length (show n) == 1 = "0" ++ (show n)
         | otherwise = show n


-- step one: filter out all non activities -- pull out innards to destructure
-- step two:


calculateActivityTime :: (Activity String) -> (Activity String) -> (Activity String)
calculateActivityTime (Activity h m t) (Activity h' m' t') =
  activityWithDuration h m h' m' t where
    activityWithDuration (Hours x) (Minutes y) (Hours x') (Minutes y') (Title z) =
      Activity (Hours h'') (Minutes m'') (Title z) where
        diff = diffUTCTime (understandTime (x <> ":" <> y)) (understandTime (x' <> ":" <> y'))
        h'' = show2d $ (if abs diff < 3600 then 0 else abs $ floor (diff) `div` 60 `div` 60)
        m'' = show2d $ (if abs diff < 60 then 0  else abs $ floor (diff) `div` 60 `rem` 60)

-- calculateActivityTimes :: [Line] -> [(Activity String)]
-- calculateActivityTimes list = foldr timeTillNext [] list where
--                                 timeTillNext (LineActivity x) b = 
--                                   (calculateActivityTime x (next index)) : b where
--                                     index         = elemIndex (LineActivity x) list
--                                     midnight      = Activity (Hours "24") (Minutes "00") (Title "None")
--                                     next (Just z) = if (length list - 1) == z then midnight else inner $ list !! (z + 1) where
--                                                       inner (LineActivity y) = y
--                                     next Nothing  = midnight
--                                 timeTillNext (LineComment x) b  = b
--                                 timeTillNext (LineDate x) b     = b

calculateActivityTimes :: [(Activity String)] -> [(Activity String)]
calculateActivityTimes list = foldr timeTillNext [] list where
                                timeTillNext a b =
                                  (calculateActivityTime a (next index)) : b where
                                    index = elemIndex a list
                                    midnight = Activity (Hours "24") (Minutes "00") (Title "None")
                                    next (Just z) = if (length list - 1) == z then midnight else list !! (z + 1)

isActivity :: Line -> Bool
isActivity (LineActivity x) = True
isActivity _                = False

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

wow = (parseString parseLogFile mempty logFile)

filtered = (filter isActivity) <$> ((!! 0) <$> wow)

main :: IO()
main = do
  -- print $  (parseString parseLogFile mempty logFile)
  -- print $ (!! 0) <$> wow
  -- print $ filtered
  print $ calculateActivityTimes <$$> (getActivity <$$> (filter isActivity) <$$> (parseString parseLogFile mempty logFile))

  -- print $ printActivity <$$> sumActivityTimes <$> Data.List.concat <$> (fmap . fmap) (calculateActivityTimes) (parseString parseLogFile mempty logFile)





