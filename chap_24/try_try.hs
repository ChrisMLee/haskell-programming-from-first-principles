{-# LANGUAGE QuasiQuotes #-}

module TryTry where

import Control.Applicative
import Data.Ratio ((%), Ratio(..))
import Text.RawString.QQ
import Text.Trifecta

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

eitherOr :: String
eitherOr = [r|
123
4/2
456
1/2|]

type DecOrFraction =
  Either Integer (Ratio Integer)

parseFraction' :: Parser Rational
parseFraction' = try $ do 
  x <- parseFraction
  return x

-- parseDecOrFraction :: Parser DecOrFraction
parseDecOrFraction = do
  skipMany (oneOf "\n")
  v <- (Right <$> try parseFraction) <|> (Left <$> try integer)
  skipMany (oneOf "\n")
  return v

-- factor = try (Unary <$> basic_char <*> unary_op)
--     <|> basic_char
-- https://github.com/forestbelton/revm/blob/19342bde3e5328aa61e75ddb6b5e6d324b3af0dd/src/RE/Parse.hs

-- (Right <$> parseFraction) <|> (Left <$> integer)
-- (Left <$> integer) 
-- <|> (Right <$> parseFraction)
      

      -- (try parseFraction) <|> parseInt

-- <|> integer

main :: IO()
main = do
  print $ parseString (some parseDecOrFraction) mempty eitherOr

