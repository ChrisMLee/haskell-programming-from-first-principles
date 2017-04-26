module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1' >>= (eof >>) . pure

one''' = do
  c <- char '1'
  eof
  return c

one'' :: Parser Char
one'' = char '1' >> eof >> return '1'

-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2' >>= (eof >>) . pure

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop


oneTwoThree =  char '1' >> char '2' >> char '3' >>= (eof >>) . pure 

-- parseString :: Parser a -> Text.Trifecta.Delta.Delta -> String -> Result a

-- eof :: Parsing m => m ()

stringP :: String -> Parser String
stringP xs = traverse char xs

multiParse = string "1" >> string "2" >> string "3" :: Parser String

testParse :: Parser Char -> IO()
testParse p =
  print $ parseString p mempty "123"

testParseTwo :: Parser String -> IO()
testParseTwo p =
  print $ parseString p mempty "123"

pNL s = 
  putStrLn ('\n' : s)

main = do
  pNL "oneTwoThree:"
  testParse oneTwoThree
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo"
  testParse oneTwo
  pNL "oneTwo'"
  testParse oneTwo'
  pNL "multiParse:"
  testParseTwo multiParse
