module UsCanada where

import Text.Trifecta
import Data.Char
import Data.List
import Control.Applicative

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

-- the goal of fmapping is to leave the outer structure untouched while transforming the type arguments inside

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

digitInt :: Parser Int 
digitInt = digitToInt <$> digit

parseThree :: Parser Int
parseThree = do
  one <- digitInt
  two <- digitInt
  three <- digitInt
  return (foldl' (\b a -> a + (b * 10)) 0 [one, two, three])

parseFour :: Parser Int
parseFour = do
  one <- digitInt
  two <- digitInt
  three <- digitInt
  four <- digitInt
  return (foldl' (\b a -> a + (b * 10)) 0 [one, two, three,four])

countryCode :: Parser Char
countryCode = do
  d <- digit
  c <- char '-'
  return c

parseStandard :: Parser PhoneNumber 
parseStandard = do
  skipMany (oneOf "(")
  numberingPlanArea <- parseThree
  skipMany (oneOf ")")
  skipMany (oneOf "-")
  skipMany (oneOf " ")
  exchange <- parseThree
  skipMany (oneOf "-")
  lineNumber <- parseFour
  eof
  return (PhoneNumber numberingPlanArea exchange lineNumber)

parseWithCountry :: Parser PhoneNumber
parseWithCountry = do
  _ <- countryCode
  rest <- parseStandard
  pure rest

parsePhone :: Parser PhoneNumber
parsePhone = do
  pn <- try parseWithCountry <|> parseStandard
  pure pn

main :: IO ()
main = do
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"




