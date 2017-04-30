module NumberOrString where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ
import Data.Scientific (floatingOrInteger)

data NumberOrString =
    Numba Integer
  | Stringy Text
  deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) = 
    case floatingOrInteger i of
      (Left _) -> fail "Must be integral number"
      (Right integer) -> return $ Numba integer
  parseJSON (String s) = return $ Stringy s
  parseJSON _ = 
    fail "NumberOrString must be number or string"

-- so it knows what we want to parse
dec :: ByteString 
    -> Maybe NumberOrString
dec = decode

-- decodes a list of strings: dec' "[\"no\",\"spray\",\"tan\"]" -> Right [Stringy "no",Stringy "spray",Stringy "tan"]
-- decodes a list of numbers: dec' "[1,2,3]" -> Right [Numba 1,Numba 2,Numba 3]
dec' :: ByteString -> Either String [NumberOrString] 
dec' = eitherDecode

eitherDec :: ByteString
          -> Either String NumberOrString
eitherDec = eitherDecode

main :: IO()
main = do
  print $ dec "blah"
  print $ eitherDec "blah"