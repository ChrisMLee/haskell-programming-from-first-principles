module ChapNine where
  import Data.Char

  filterUppers:: [Char] -> [Char]
  filterUppers xs = filter isUpper xs

  firstLetterCaps:: [Char] -> [Char]
  firstLetterCaps xs = toUpper (head xs) : tail xs

  capAll:: [Char] -> [Char]
  capAll xs = map toUpper xs

  
  firstAndCap::[Char] -> Char
  firstAndCap = (toUpper . head)