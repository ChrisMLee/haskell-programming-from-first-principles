import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case ( (formatInput line1) == (reverse $ formatInput line1) ) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess


--foldr (\a b -> if a == ' ' || elem a "’" then b else a:b) "" "hello world"

formatInput:: [Char] -> [Char]
formatInput = map toLower . foldr (\a b -> if a == ' ' || elem a "’" then b else a:b) ""