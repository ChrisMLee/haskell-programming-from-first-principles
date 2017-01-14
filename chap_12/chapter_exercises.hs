module ChapterExercises where

-- String Processing

notThe :: String -> Maybe String
notThe word
  | word /= "the" = Just word
  | otherwise = Nothing

replaceThe :: String -> String
replaceThe = unwords . map handleNothing . map notThe . words
              where handleNothing x = 
                      case x of
                        Nothing -> "a"
                        Just i -> i

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel x = countTheBeforeVowel (words x) 0 where
  countTheBeforeVowel [] acc = acc
  countTheBeforeVowel (y:ys) acc = if y == "the" && elem (head $ (head ys)) "aeiou" then countTheBeforeVowel (tail $ tail ys) ((+) acc 1) else countTheBeforeVowel ys acc

-- interesting: when it recurses, it doesn't touch the statement before where

countVowels :: String -> Integer
countVowels = toInteger . length . filter id . map (flip elem "aeiou")

countConsonants :: String -> Integer
countConsonants = toInteger . length . filter (not . id) . map (flip elem "aeiou")

-- Validate the word

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"
mkWord :: String -> Maybe Word'
mkWord x =  if countVowels x > countConsonants x then Nothing else Just (Word' x)

-- It's only natural

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = (+) 1 (natToInteger n)

-- This wasn't quite working:
--integerToNat (n < 0) = Nothing
--integerToNat 0 = Zero
--integerToNat 1 = Succ Zero
--integerToNat n = Succ(integerToNat (n-1))

-- via https://github.com/dwayne/haskell-programming/blob/master/ch12/Natural.hs
-- when you need a recursive function tucked inside a guard
integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just $ go n where
    go 0 = Zero
    go n = Succ $ go $ n-1

-- Small library for Maybe
--1.

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

--2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x y Nothing = x
mayybee x y (Just z) = y z

--3. 
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = mayybee x id Nothing
fromMaybe x (Just y) = mayybee x id (Just y)

--4.

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

--5. 
catMaybes :: [Maybe a] -> [a]
catMaybes = map returnContents . filter check
              where check x = 
                      case x of
                        Nothing -> False
                        Just i -> True

                    returnContents (Just y) = y

--6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if length (catMaybes xs) == (length xs) then (Just $ catMaybes xs) else Nothing

-- Small library for Either


lefts' :: [Either a b] -> [a]
lefts' = foldr getLeft []
          where getLeft a b = 
                  case a of 
                    (Right _) -> b
                    (Left x) -> x:b

rights' :: [Either a b] -> [b]
rights' = foldr getRight []
          where getRight a b = 
                  case a of 
                    (Left _) -> b
                    (Right x) -> x:b

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = ((lefts' xs), (rights' xs))

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x)= Just (f x)
eitherMaybe' f (Left _) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f f' (Left x) = f x
either' f f' (Right x) = f' x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e  = either' (\x -> Nothing) (Just . f) e

--eitherMaybe'' f (Right x) = Just $ either' id f (Right x)
--eitherMaybe'' f (Left _) = Nothing 

--Write your own iterate and unfoldr
--1.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:(myIterate f (f x))

-- Failed attempt
--myIterate f x = go f x [] where
--  go f x acc = go f (f x) (x:acc)

--2. 
--myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
--myUnfoldr f x = a:(myUnfoldr f b) where
--    Just (a,b) = f x
--    Nothing = Nothing
-- via https://github.com/mikowitz/haskell-from-first-principles/blob/master/chapter12/Unfolds.hs
-- WRONG ^

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                Nothing -> []
                Just (x, y) -> x : myUnfoldr f y
-- via https://lukleh.github.io/haskell-book-exercises/#_12_5_chapter_exercises

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just(b, (f b))) x

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f b = case f b of
             Nothing -> Leaf
             Just(left, x, right) -> Node (unfold f left) x (unfold f right)


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\b -> if b < 2^n - 1
                                   then Just (2*b+1, b, 2*b+2)
                                   else Nothing) 0
--"The way I arrived at this is by drawing a binary tree with depth 3. 
--I numbered the nodes starting with the root as 0, the left node as 1 and the right node as 2. The bottom nodes are numbered from left to right starting with 4 and ending at 7."

--"Now the pattern is visible: if the current node is numbered b, his left and right nodes are numbered 2*b+1 and 2*b+2 respectively. Since 2^n - 1 is the total number of nodes in a tree of depth n, and I'm numbering nodes in breadth-first order, returning Nothing when b >= 2^n-1 ensures I stop after filling the tree up to depth n."

-- via http://stackoverflow.com/a/15538664

-- Node left a right

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)













