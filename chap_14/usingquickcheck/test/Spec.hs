import Test.QuickCheck
-- import Test.QuickCheck.Instances.List (anyList)
import Test.QuickCheck.Modifiers (NonZero)
import Test.QuickCheck.Function (apply, Fun(..))
import Data.List (sort)
import Data.Char

main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multiplyAssociative
  quickCheck prop_multiplyCommutative
  quickCheck prop_quotRemLaws
  quickCheck prop_exponentialAssociative
  quickCheck prop_exponentialCommutative
  quickCheck prop_doubleReverse
  quickCheck prop_apply
  quickCheck consEqualsAppend
  quickCheck appendEqualsConcat
  quickCheck prop_take
  quickCheck prop_readShow
  quickCheck prop_capitalizeWordIdempotent
  quickCheck prop_sortIdempotent

-- 1.
-- for a function
half :: Fractional a => a -> a
half x = x / 2

-- this property should hold
halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_halfIdentity :: Float -> Bool
prop_halfIdentity x = halfIdentity x == x

-- 2.

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool 
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

--prop_listOrdered :: Property
--prop_listOrdered = forAll (arbitrary :: Gen ([Int])) (listOrdered . sort)

prop_listOrdered :: [Int] -> Bool
prop_listOrdered xs = (listOrdered (sort xs)) == True

-- 3. 

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

--prop_plusAssociative = forAll (arbitrary :: Gen (Int)) (plusAssociative)

prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative = plusAssociative

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative = plusCommutative

--prop_plusCommutative = forAll (arbitrary :: Gen (Int)) (plusCommutative)

-- 4. 
multiplyAssociative x y z = x * (y * z) == (x * y) * z
multiplyCommutative x y z = x * y * z == z * y * x

--prop_multiplyAssociative = forAll (arbitrary :: Gen (Int)) (multiplyAssociative)
--prop_multiplyCommutative = forAll (arbitrary :: Gen (Int)) (multiplyCommutative)

prop_multiplyAssociative :: Int -> Int -> Int -> Bool
prop_multiplyAssociative = multiplyAssociative

prop_multiplyCommutative:: Int -> Int -> Int -> Bool
prop_multiplyCommutative = multiplyCommutative


-- 5. 

-- see SO post for how to generate nonZero arbitrary values
-- http://stackoverflow.com/questions/35425002/remove-zero-from-infinite-float-list/35425124

prop_quotRemLaws :: Property
prop_quotRemLaws = 
  forAll nonZeroInt $ \x -> forAll nonZeroInt $ \y ->  (quot x y) * y + (rem x y) == x

nonZeroInt :: Gen Int
nonZeroInt = fmap getNonZero arbitrary

prop_divModLaws :: Property
prop_divModLaws = forAll nonZeroInt $ \x -> forAll nonZeroInt $ \y -> (div x y)*y + (mod x y) == x

-- 6.

-- Is (^) associative? Is it commutative?
exponentialAssociative :: Int -> Int -> Int -> Bool
exponentialAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

exponentialCommutative :: Int -> Int -> Int -> Bool
exponentialCommutative x y z = x ^ y ^ z == z ^ x ^ y

prop_exponentialAssociative :: Property
prop_exponentialAssociative = forAll (arbitrary :: Gen (Int)) exponentialAssociative

prop_exponentialCommutative :: Property
prop_exponentialCommutative = forAll (arbitrary :: Gen (Int)) exponentialCommutative

--7 .
-- Test that reversing a list twice is the same as the identity of the list
prop_doubleReverse :: Property
prop_doubleReverse = forAll (arbitrary :: Gen ([Int])) (\xs -> (reverse $ reverse xs) == (id xs))

-- 8. Write a property for the definition of ($).

-- Fun (a :-> b, b, Bool) (a -> b)  

prop_apply :: Fun Int Int -> Int -> Bool
prop_apply (Fun _ f) a = (f $ a) == (f a)

prop_compose :: Fun Int Int -> Fun Int Int -> Int -> Bool 
prop_compose (Fun _ f) (Fun _ g) x = (f . g) x == (f (g x))

-- 9. cons vs append
consEqualsAppend :: [Int] -> [Int] -> Bool
consEqualsAppend xs ys = (foldr (:) xs ys) == ((++) ys xs)

appendEqualsConcat :: [[Int]] -> Bool
appendEqualsConcat xs = (foldr (++) [] xs) == (concat xs)

-- 10.

prop_take :: Int -> [Int] -> Bool
prop_take n xs = length (take n xs) == n

-- 11.
prop_readShow :: Int -> Bool
prop_readShow x = (read (show x)) == x

--roundTrip :: (Show a, Read a) => a -> a
--roundTrip a = read (show a)

-- 11.5.

-- for a function
square :: Floating a => a -> a
square x = x * x

-- why does this property not hold? Examine the type of sqrt.
-- a: sqrt uses a Floating type constraint
squareIdentity = square . sqrt


-- Idempotence
twice f = f . f
fourTimes = twice . twice
-- 1.

capitalizeWord :: [Char] -> [Char]
capitalizeWord = map toUpper

prop_capitalizeWordIdempotent:: [Char] -> Bool
prop_capitalizeWordIdempotent x = 
  (capitalizeWord x 
  == twice capitalizeWord x) &&
  (twice capitalizeWord x == fourTimes capitalizeWord x)

--2. 

prop_sortIdempotent:: [Int] -> Bool
prop_sortIdempotent x =
  (sort x
  == twice sort x) &&
  (twice sort x == fourTimes sort x)


-- Make a Gen random generator for the datatype
-- 1. Equal probabilities for each.
data Fool = 
    Fulse
  | Frue
  deriving (Eq, Show)

foolGenEqual :: Gen Fool
foolGenEqual = do
  frequency [(1, return $ Fulse),
             (1, return $ Frue)]

foolGenTwoThirds :: Gen Fool
foolGenTwoThirds = do
  frequency [(2, return $ Fulse),
             (1, return $ Frue)]












