module Cipher where
import Data.Char
import System.IO

shiftChar:: Int -> Char -> Char
shiftChar shift x =
  case ((+) shift $ ord x) > 122 of
    True -> chr . (+) 96 $ flip mod 122 $ ord x + shift
    False -> chr . (+) shift $ ord x

runCipher:: String -> Int -> String
runCipher xs shift =  map (shiftChar shift) xs

--main :: IO ()
--main = do
--    putStr "Input the string you want to encrypt: "
--    word <- getLine 
--    putStrLn (show $ runCipher word 1)

main :: IO ()
main = do
  putStr "input caesar shift: "
  shift <- getLine
  putStr "input text: "
  str <- getLine
  putStrLn $ "encrypted text: " ++ runCipher str (read shift :: Int)

-- either mod 122 or 97

-- 122 + 5 should equal 5
-- 97-3 should equal 

