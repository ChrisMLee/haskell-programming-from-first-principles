module UnitOfSuccess where

import Text.Trifecta

unitOfSuccess :: Parser Integer
unitOfSuccess = do
  numInput <- integer
  eof
  return numInput