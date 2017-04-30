{-# LANGUAGE OverloadedStrings #-}

module BT where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

trifP :: Show a
      => Parser a
      -> String -> IO ()
trifP p i = 
  print $ parseString p mempty i

parsecP :: (Show a)
        => Parsec String () a
        -> String -> IO ()
parsecP = parseTest

-- attoP :: Show a => A.Parser a -> ByteString -> IO()
attoP p i = 
  print $ parseOnly p i

noBackParse :: (Monad f, CharParsing f)
            => f Char
noBackParse =
       (char '1' >> char '2')
  <|> char '3'

tryParse :: (Monad f, CharParsing f)
         => f Char
tryParse = 
       try (char '1' >> char '2')
   <|> char '3'

-- This parser has similar behavior to the previous one except it
-- backtracks if the first parse fails. What backtracking means here
-- is that the input cursor returns to where it was before the failed
-- parser consumed input

main :: IO ()
main = do 
  -- trifecta
  trifP noBackParse "13"
  trifP tryParse "13"
  -- parsec
  parsecP noBackParse "13"
  parsecP tryParse "13"
  -- attoParsec
  -- attoP nobackParse "13"
  -- attoP tryParse "13"

tryAnnot :: (Monad f, CharParsing f) 
         => f Char
tryAnnot = 
       (try (char '1' >> char '2')
       <?> "Tried 12")
  <|>  (char '3' <?> "Tried 3")




















