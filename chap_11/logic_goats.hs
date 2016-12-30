{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool


newtype Goats =
  Goats Int deriving (Eq, Show)

instance TooMany Int where
  tooMany x = x > 3

instance TooMany String where
  tooMany x = length x > 3

instance TooMany (Int, String) where
  tooMany (x, y) = x > (length y)
-- tooMany (5 :: Int,"string")

instance TooMany (Int, Int) where
  tooMany (a1, a2) = a1 + a2 > 43

--instance (Num a, TooMany a) => TooMany (a, a) where
--  tooMany (a1, a2) = tooMany(a1 + a2) 
-- tooMany ((1,1) :: (Int, Int))