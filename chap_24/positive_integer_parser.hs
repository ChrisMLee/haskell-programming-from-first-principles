-- Chapter Exercises: #2

module PositiveIntegerParser where

import Control.Applicative
import Text.Trifecta
import Data.Char
import Data.List

parseDigit :: Parser Char
parseDigit = char '1' <|> 
             char '2' <|>
             char '3' <|>
             char '4' <|>
             char '5' <|>
             char '6' <|>
             char '7' <|>
             char '8' <|>
             char '9' <|>
             char '0' <?>
             "parseDigit"

-- parseDigit' :: Parser Char
-- parseDigit' = oneOf ['0'..'9'] <?> "parse base 10 digits"

parseDigit'' :: Parser Integer
parseDigit'' = do
  d <- oneOf ['0'..'9']
  pure (toInteger $ digitToInt d)

base10Integer :: Parser Integer
base10Integer = do
  i <- (some parseDigit'') <?> "integer"
  pure (foldl (\b a -> a + (b * 10) ) 0 i)

base10Integer' :: Parser Integer
base10Integer' = do
  skipMany (oneOf "-")
  i <- (some parseDigit'') <?> "integer"
  return (foldl (\b a -> a + (b * 10) ) 0 i)

-- base10Integer'' :: Parser Integer
-- base10Integer'' = do 
--   intArr <- p'
--   return (foldr (a) intArr 

-- p' :: Parser [Integer] 
-- p'= some $ do
--   i <- (some parseDigit)
--   return (read i)

-- parseWithMessage = parseDigit' <?> "integer"

-- length - 1 to zeros. 1 * 100, 2 * 10, 3 * 1
-- parseString (some parseDigit'') mempty "123abc" -> Success [1,2,3]
-- foldr (\a b -> b + (a * 10) ) 0 [1,2,3]

main :: IO()
main = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"
  print $ parseString base10Integer' mempty "-123abc"




