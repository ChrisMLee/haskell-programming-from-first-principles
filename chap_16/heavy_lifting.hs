module HeavyLifting where
import Data.Maybe

--1.
a = fmap (+1) (read "[1]" :: [Int])

--2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

--3.

c x = fromJust $ (fmap (*2) (fmap (\x -> x - 2) (Just x) ))
-- fmap (*2) (\x -> x - 2)
-- c = (*2) . (\x -> x -2)

-- fmap is function composition


--4.
d x = fromJust $ fmap ( ((return '1' ++) . show) . (\x -> [x, 1..3])) (Just x)

-- d = fmap ((return '1' ++) . show)(\x -> [x, 1..3])

--5.
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (\x -> read x :: Integer) ((fmap (("123"++) . show) ioi))
    in fmap (*3) changed



--fmap show (readIO "1" :: IO Integer)
-- fmap show ioi