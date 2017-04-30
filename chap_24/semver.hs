module Semver where

import Control.Applicative
import Text.Trifecta
import Data.Monoid
import Control.Monad

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString =
    NOSS String
  | NOSI Integer 
  deriving (Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = 
  SemVer Major Minor Patch Release Metadata deriving (Show)

parseNumOrString :: Parser NumberOrString
parseNumOrString = 
  try (NOSI <$> integer) <|> (NOSS <$> some letter)

parseRelease :: Parser Release
parseRelease = parseFullRelease <|> parsePartialRelease <|> return []

parsePartialRelease :: Parser Release
parsePartialRelease = try $ do
  _ <- char '-'
  a <- parseNumOrString
  return [a]

parseFullRelease :: Parser Release
parseFullRelease =  try $ do
  _ <- char '-'
  a <- parseNumOrString
  skipMany (oneOf ".")
  b <- parseNumOrString
  skipMany (oneOf ".")
  c <- parseNumOrString
  skipMany (oneOf ".")
  d <- parseNumOrString
  return [a, b, c, d]

parseMetaData = parseFullMetaData <|> parsePartialMetadata <|> return []

parseFullMetaData :: Parser Metadata
parseFullMetaData = try $ do
  a <- parseNumOrString
  skipMany (oneOf ".")
  b <- try parseNumOrString
  skipMany (oneOf ".")
  c <- try parseNumOrString
  return [a,b,c]

parsePartialMetadata :: Parser Metadata
parsePartialMetadata = try $ do
  a <- parseNumOrString
  return [a]


parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  skipMany (oneOf ".")
  minor <- decimal
  skipMany (oneOf ".")
  patch <- decimal
  release <- parseRelease
  skipMany (oneOf "+")
  metadata <- parseMetaData
  return (SemVer major minor patch release metadata)

-- x.7.z.92

main :: IO()
main = do
  print $ parseString parseSemVer mempty "2.1.1"
  print $ parseString parseRelease mempty "x.7.z.92"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  print $ parseString parseSemVer mempty "1.0.0-alpha+001"
  print $ parseString parseSemVer mempty "1.0.0+20130313144700"
  print $ parseString parseSemVer mempty "1.0.0-beta+exp.sha.5114f85"


-- 1.0.0-alpha+001
-- 1.0.0+20130313144700
-- 1.0.0-beta+exp.sha.5114f85





