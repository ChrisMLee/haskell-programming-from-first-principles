module Cipher where
  import Data.Char

  shiftChar:: Int -> Char -> Char
  shiftChar shift x =
    case ((+) shift $ ord x) > 122 of
      True -> chr . (+) 96 $ flip mod 122 $ ord x + shift
      False -> chr . (+) shift $ ord x

  runCipher:: String -> Int -> String
  runCipher xs shift =  map (shiftChar shift) xs

  main :: IO ()
  main =
    print $ runCipher "hello" 1

  -- either mod 122 or 97

  -- 122 + 5 should equal 5
  -- 97-3 should equal 