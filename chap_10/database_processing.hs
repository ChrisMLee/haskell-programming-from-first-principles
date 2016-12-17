module DataProcessing where
  import Data.Time
  data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate   UTCTime
                    deriving (Eq, Ord, Show)


  theDatabase :: [ DatabaseItem ]
  theDatabase =
    [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123)) 
    , DbNumber 9001
    , DbNumber 3
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

  isDbDate :: DatabaseItem -> Bool
  isDbDate (DbDate _) = True
  isDbDate _     = False

  isDbNumber :: DatabaseItem -> Bool
  isDbNumber (DbNumber _) = True
  isDbNumber _ = False

  dbDateContents :: DatabaseItem -> UTCTime
  dbDateContents (DbDate val) = val

  dbNumberContents :: DatabaseItem -> Integer
  dbNumberContents (DbNumber val) = val

  filterDbDate :: [DatabaseItem] -> [UTCTime]
  filterDbDate xs =  map dbDateContents $ filter isDbDate xs

  filterDbNumber :: [DatabaseItem] -> [Integer]
  filterDbNumber xs = map dbNumberContents $ filter isDbNumber xs

  mostRecent :: [DatabaseItem] -> UTCTime
  mostRecent xs = foldr1 (\x acc -> if x > acc then x else acc) (filterDbDate xs)

  sumDb :: [DatabaseItem] -> Integer
  sumDb xs = foldr (+) 0 (filterDbNumber xs)


  avgDb :: [DatabaseItem] -> Double
  avgDb xs = (/) (fromIntegral $ sumDb xs) (fromIntegral . length $ filterDbNumber xs)


