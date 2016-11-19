module Symmetry where
  myWords:: String -> Char -> [String]
  myWords x p =  reverse (go x []) where
    go x' xs = case (dropWhile(== p) x') of
                  "" -> xs
                  anythingyouwant -> go ((dropWhile (/= p) anythingyouwant)) ((takeWhile (/= p) anythingyouwant) : xs)

  -- This composition operator, (.). in elm (<<)

  --  Prelude> negate . sum $ [1, 2, 3, 4, 5]
  -- -15




  --myWords :: String -> [String]
  --myWords [] = []
  --myWords s = beforeSpace s : myWords (afterSpace s)
  --  where beforeSpace = takeWhile (/= ' ')
  --        afterSpace  = dropWhile (== ' ') . dropWhile (/= ' ')

  --takeWhile (==' ') x

  --wordsBy :: String -> Char -> [String]
  --wordsBy s c = reverse (go s []) where
  --    go s' ws = case (dropWhile (\c' -> c' == c) s') of
  --        "" -> ws
  --        rem -> go ((dropWhile (\c' -> c' /= c) rem)) ((takeWhile (\c' -> c' /= c) rem) : ws)



  --dropWhile(==' ')

  --takeWhile (/=' ')



  --dividedBy :: Integral a => a -> a -> (a, a) dividedBy num denom = go num denom 0
  --  where go n d count
  --  | n < d = (count, n)
  --  | otherwise = go (n - d) d (count + 1)