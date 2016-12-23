{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43


-- class TooMany a where
--   tooMany :: a -> Bool

-- instance TooMany Int where
--   tooMany x = x > 3

-- instance TooMany String where
--   tooMany x = length x > 3

-- instance TooMany (Int, String) where
--   tooMany (x, y) = x > (length y)
-- -- tooMany (5 :: Int,"string")

-- instance (Num a, TooMany a) => TooMany (a, a) where
--   tooMany (a1, a2) = tooMany(a1 + a2) 
-- -- tooMany ((1,1) :: (Int, Int))