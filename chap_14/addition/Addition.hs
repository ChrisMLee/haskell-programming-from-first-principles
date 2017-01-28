module Addition where
import Test.Hspec
import Test.QuickCheck


--main :: IO () 
--main = hspec $ do
--  describe "Addition" $ do
--    it "15 divided by 3 is 5" $ do
--      dividedBy 15 3 `shouldBe` (5,0)
--    it "22 divided by 5 is 4 remainder 2" $ do
--      dividedBy 22 5 `shouldBe` (4,2)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

--Write a function that recursively sums all numbers from 1 to n, n being the argument.
--So that if n was 5,you’d add 1+2+3+4+5 to get 15. The type should be(Eq a, Num a) => a -> a.


sumAll::(Eq a, Num a) => a -> a
sumAll n = sumAll n 0 where
  sumAll 0 acc = acc
  sumAll n acc = sumAll (n-1) (acc + n)

main :: IO () 
main = hspec $ do
  describe "Addition" $ do
    it "given 5 returns 15" $ do
      sumAll 5 `shouldBe` 15
    it "given 9 returns 45" $ do
      sumAll 9 `shouldBe` 45
    it "x + 1 is always greater than x" $ do 
      property $ \x -> x + 1 > (x :: Int)


oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 2, 2, 2, 3]


genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']


genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) =>
               Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe'::Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
