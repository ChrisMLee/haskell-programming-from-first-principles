module LetsWriteCode where
  tensDigit :: Integral a => a -> a
  tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

  tensDigitDm:: Integral a => a -> a
  tensDigitDm x = (fst . (flip divMod)(10)) x

  hunsD:: Integral a => a -> a
  hunsD x = (fst . (flip divMod)(100)) x

  foldBool1::a -> a -> Bool -> a
  foldBool1 x y bool
    | bool == True = x 
    | bool == False = y
    | otherwise = y

  foldBool2:: a -> a -> Bool -> a
  foldBool2 x y bool = 
    case bool of
      True -> x
      False -> y


  foldBool3 :: a -> a -> Bool -> a
  foldBool3 x y True = x
  foldBool3 x y False = y

