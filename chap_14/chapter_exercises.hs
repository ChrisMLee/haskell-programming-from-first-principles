module ChapterExercises where
import Test.QuickCheck

main :: IO ()
main = do
  it "x + 1 is always greater than x" $ do
    property $ \x -> x + 1 > (x :: Int)

-- 1. -- for a function
half x = x / 2

-- this property should hold
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = (half x) == (halfIdentity x)




-- for a function
square x = x * x

-- why does this property not hold? Examine the type of sqrt.
squareIdentity = square . sqrt