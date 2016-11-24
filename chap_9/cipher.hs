module Cipher where
  import Data.Char

  shiftChar:: Char -> Int -> Char
  shiftChar x shift =
    case ((+) shift $ ord x) > 122 of
      True -> chr . (+) 96 $ flip mod 122 $ ord x + shift
      False -> chr . (+) shift $ ord x

  -- either mod 122 or 97

  -- 122 + 5 should equal 5
  -- 97-3 should equal 