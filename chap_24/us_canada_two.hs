{-# LANGUAGE DeriveFunctor #-}

module UsCanadaTwo where

import Text.Trifecta
import Data.List
import Data.Char
import Control.Applicative

data PhoneNumber a =
  PhoneNumber (AreaCode a) (Prefix a) (LineNumber a)
  deriving (Eq, Show, Functor)

newtype AreaCode   a = AreaCode   a deriving (Eq, Show, Functor)
newtype Prefix     a = Prefix     a deriving (Eq, Show, Functor)
newtype LineNumber a = LineNumber a deriving (Eq, Show, Functor)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (fmap . fmap)

base10Conv :: [Int] -> Int
base10Conv = foldl' (\b a -> a + (b * 10)) 0

parseAreaCode :: Parser (AreaCode String)
parseAreaCode = do
  a <- count 3 digit
  return (AreaCode a)

parsePrefix :: Parser (Prefix String)
parsePrefix = do
  a <- count 3 digit
  return (Prefix a)

parseLineNumber :: Parser (LineNumber String)
parseLineNumber = do
  a <- count 4 digit
  return (LineNumber a)

countryCode :: Parser Char
countryCode = do
  d <- digit
  c <- char '-'
  return c

skipSeperatorChars :: Parser ()
skipSeperatorChars =
  skipMany (char '-' <|> char ')' <|> char '(' <|> char ' ')

parseCountryCode :: Parser Char
parseCountryCode = try countryCode <|> return '-'

parsePhoneNumber :: Parser (PhoneNumber Int)
parsePhoneNumber = do
  _ <- parseCountryCode
  skipSeperatorChars
  areaCode <- parseAreaCode
  skipSeperatorChars
  prefix <- parsePrefix
  skipSeperatorChars
  lineNumber <- parseLineNumber
  return ( fmap base10Conv $ (digitToInt <$$> PhoneNumber areaCode prefix lineNumber))

main :: IO()
main = do
  print $ parseString parsePhoneNumber mempty "123-456-7890"
  print $ parseString parsePhoneNumber mempty "1234567890"
  print $ parseString parsePhoneNumber mempty "(123) 456-7890"
  print $ parseString parsePhoneNumber mempty "1-123-456-7890"






