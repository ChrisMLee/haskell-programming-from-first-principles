{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}

module LogActivity where

import Text.RawString.QQ
import Text.Trifecta
import Data.List
import Control.Applicative
import Data.Time
import Data.Semigroup (Semigroup, (<>))
import Data.Char

-- http://ekmett.github.io/trifecta/

newtype Hours a = Hours a deriving (Eq, Show, Functor)
newtype Minutes a = Minutes a deriving (Eq, Show, Functor)
newtype Title a = Title a deriving (Eq, Show, Functor)

data Activity a =
  Activity (Hours a) (Minutes a) (Title a) 
  deriving (Eq, Show, Functor)

getActivityTime :: (Activity String) -> String 
getActivityTime (Activity h m t) = getActivityTime h m where
                                    getActivityTime (Hours x) (Minutes y) = x <> ":" <> y

getActivityTitle :: (Activity String) -> String
getActivityTitle (Activity h m t) = getActivityTitle t where
                                      getActivityTitle (Title x) = x

getActivityHours :: (Activity String) -> String
getActivityHours (Activity h m t) = getActivityHours h where
                                      getActivityHours (Hours x) = x

getActivityMinutes :: (Activity String) -> String
getActivityMinutes (Activity h m t) = getActivityMinutes m where
                                      getActivityMinutes (Minutes x) = x

newtype Comment = Comment String deriving (Eq, Show)
-- Todo: change to a type with a month, date, year
newtype Date = Date String deriving (Eq, Show)

data Line = 
    LineDate Date
  | LineComment Comment
  | LineActivity (Activity String)
  deriving (Eq, Show)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (fmap . fmap)

-- convert this to a Show (Activity String) instance?
printActivity :: (Activity String) -> String
printActivity (Activity h m t) = printActivity h m t where
                                  printActivity (Hours x) (Minutes y) (Title z) =
                                    z <> " - " <> x <> ":" <> y

base10Conv :: [Int] -> Int
base10Conv = foldl' (\b a -> a + (b * 10)) 0

parseActivity :: Parser (Activity String)
parseActivity = do
  h <- count 2 digit
  skipMany (oneOf ":")
  m <- count 2 digit
  t <- space *> manyTill anyChar (try (string "\n") <|> (string " --" *> space *> manyTill anyChar (try (string "\n"))))
  return (Activity (Hours h) (Minutes m) (Title t))

parseComment :: Parser Comment
parseComment = Comment <$> (string "--" *> space *> manyTill anyChar (try (string "\n")))

parseDate :: Parser Date
parseDate = Date <$> (char '#' *> space *> manyTill anyChar (try (string "\n")))

parseWord :: Parser [[Char]]
parseWord = try (some (token $ some letter)) <|> return []

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

-- commment, date, line
parseLine :: Parser Line
parseLine = try (LineDate <$> parseDate) <|> (LineComment <$> parseComment) <|> (LineActivity <$> parseActivity)

parseLogFile :: Parser [[Line]]
parseLogFile = do
  skipWhitespace
  thangs <- some (token parseSection)
  return thangs

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-- parseSection :: Parser Line
parseSection = do
  skipMany parseComment
  date <- LineDate <$> parseDate
  lines <- some parseLine
  return (date : lines)

-- via http://stackoverflow.com/questions/376968/convert-haskell-int-with-leading-zero-to-string/376982#376982
show2d :: Int -> String 
show2d n | length (show n) == 1 = "0" ++ (show n)
         | otherwise = show n

-- via http://stackoverflow.com/a/39818693
timeFormat :: String
timeFormat = "%H:%M"
understandTime = parseTimeOrError True defaultTimeLocale timeFormat

-- http://stackoverflow.com/questions/25806378/how-do-i-convert-difftime-to-nominaldifftime

formatMinutes :: UTCTime -> String
formatMinutes = formatTime defaultTimeLocale "%M"

formatHours :: UTCTime -> String
formatHours = formatTime defaultTimeLocale "%H"

-- http://stackoverflow.com/questions/14656762/no-instance-for-fractional-int-arising-from-a-use-of
-- divideActivityTime :: Integer -> (Activity String) -> (Activity String)
averageActivityTime n a = 
   Activity (Hours h') (Minutes m') (Title t) where
    t = getActivityTitle a
    h = getActivityHours a
    m = getActivityMinutes a
    timeInSeconds = ((base10Conv $ map digitToInt h) * 60 * 60) + ((base10Conv $ map digitToInt m) * 60)
    averageTime   = timeInSeconds `div` n
    h' = show2d $ averageTime `div` 60 `div` 60
    m' = show2d $ averageTime `div` 60 `rem` 60

-- Activity (Hours h') (Minutes m') (Title t) 

combineActivities :: (Activity String) -> (Activity String) -> (Activity String)
combineActivities a a' =
  Activity (Hours h'') (Minutes m'') (Title t) where
    t = getActivityTitle a
    h = getActivityHours a
    m = getActivityMinutes a
    h' = getActivityHours a'
    m' = getActivityMinutes a'
    additionalSeconds = ((base10Conv $ map digitToInt h') * 60 * 60) + ((base10Conv $ map digitToInt m') * 60)
    sumTime = addUTCTime ((fromRational . toRational) additionalSeconds) (understandTime (h <> ":" <> m))
    h'' = formatHours sumTime
    m'' = formatMinutes sumTime

-- via http://stackoverflow.com/questions/31087004/print-nominaldifftime-as-hours-minutes-and-seconds
calculateActivityTime :: (Activity String) -> (Activity String) -> (Activity String)
calculateActivityTime (Activity h m t) (Activity h' m' t') = 
  activityWithDuration h m h' m' t where
    activityWithDuration (Hours x) (Minutes y) (Hours x') (Minutes y') (Title z) =
      Activity (Hours h'') (Minutes m'') (Title z) where
        diff = diffUTCTime (understandTime (x <> ":" <> y)) (understandTime (x' <> ":" <> y'))
        h'' = show2d $ (if abs diff < 3600 then 0 else abs $ floor (diff) `div` 60 `div` 60)
        m'' = show2d $ (if abs diff < 60 then 0  else abs $ floor (diff) `div` 60 `rem` 60)

calculateActivityTimes :: [Line] -> [(Activity String)]
calculateActivityTimes list = foldr timeTillNext [] list where
                                timeTillNext (LineActivity x) b = 
                                  (calculateActivityTime x (next index)) : b where
                                    index         = elemIndex (LineActivity x) list
                                    midnight      = Activity (Hours "24") (Minutes "00") (Title "None")
                                    next (Just z) = if (length list - 1) == z then midnight else inner $ list !! (z + 1) where
                                                      inner (LineActivity y) = y
                                    next Nothing  = midnight
                                timeTillNext (LineComment x) b  = b
                                timeTillNext (LineDate x) b     = b

sumActivityTimes :: [(Activity String)] -> [(Activity String)]
sumActivityTimes activityList = foldr sumFunc [] activityList where
                                  sumFunc a b = if hasTitle then 
                                      combineActivities a (b !! (getIndex matchIndex)) : filteredList else
                                      a : b where 
                                    title             = getActivityTitle a
                                    currentTitles     = getActivityTitle <$> b
                                    hasTitle          = elem title currentTitles
                                    matchIndex        = elemIndex title currentTitles
                                    filteredList      = filter (\x -> getActivityTitle x /= title) b
                                    getIndex (Just x) = x

-- total activity times / number of dates
getDates :: [Line] -> [Date]
getDates activityList = foldr getDate [] activityList where
                          getDate (LineDate x) b = x : b
                          getDate _ b  = b

sectionOne :: String
sectionOne = [r|
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

sectionTwo :: String
sectionTwo = [r|
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
  print $ "Total time spent:"
  print $ printActivity <$$> sumActivityTimes <$> Data.List.concat <$> (fmap . fmap) (calculateActivityTimes) (parseString parseLogFile mempty logFile)

  print $ "Average time spent per day:" 
  print $  printActivity <$$> ( (fmap <$> (averageActivityTime <$> (length <$> getDates <$> Data.List.concat <$> (parseString parseLogFile mempty logFile)))) <*> (sumActivityTimes <$> Data.List.concat <$> (fmap . fmap) (calculateActivityTimes) (parseString parseLogFile mempty logFile)) )

