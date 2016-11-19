module RangesLists where
  eftBool :: Bool -> Bool -> [Bool]
  eftBool False True = [False, True]
  eftBool True False = []

  eftOrd :: Ordering -> Ordering -> [Ordering]
  eftOrd LT GT = [LT, EQ, GT]
  eftOrd LT EQ = [LT, EQ]
  eftOrd GT _ = []

  eftInt :: Int -> Int -> [Int]
  eftInt x y = case compare x y of
    GT -> []
    EQ -> [x]
    LT -> x : eftInt (succ x) y

  eftChar:: Char -> Char -> [Char]
  eftChar x y = case compare x y of 
            GT -> []
            EQ -> [x]
            LT -> x : eftChar (succ x) y