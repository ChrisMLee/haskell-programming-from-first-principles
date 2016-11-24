module Cipher where
  import Data.Char

  shiftChar:: Char -> Char
  shiftChar x =
    case ((+) 5 $ ord x) > 122 of
      True -> chr . (+) 97 $ flip mod 122 $ ord x + 5
      False -> chr . (+) 5 $ ord x

  -- either mod 122 or 97

  -- 122 + 5 should equal 5
  -- 97-3 should equal 