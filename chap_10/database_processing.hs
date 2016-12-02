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
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

  isDbDate :: DatabaseItem -> Bool
  isDbDate (DbDate _) = True
  isDbDate _     = False

  --matchUTC :: DatabaseItem -> UTCTime
  --matchUTC (DbDate UTCTime) = UTCTime

  --filterDbDate :: [DatabaseItem] -> [UTCTime]
  --filterDbDate = foldr  (\a b -> if isDbDate) [] theDatabase
