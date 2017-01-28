module ChapterExercisesSemigroup where
import Test.QuickCheck
import Data.Semigroup

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)

-- 1. 
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

--2.

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary 
  return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3. 

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two z w) = Two (x <> z) (y <> w)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

--4. 

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)


instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

-- 5. 

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)


instance (Arbitrary a, Arbitrary b, Arbitrary c,  Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool

-- 6.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj (choose (True, False))

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7.
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj (if elem True [a,b] then True else False)

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj (choose (True, False))

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8.
data Or a b = 
    Fst a
  | Snd b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>  Semigroup (Or a b) where
  (Fst x) <> (Snd y) = Snd y
  (Fst x) <> (Fst y) = Fst y
  (Snd x) <> (Fst y) = Snd x
  (Snd x) <> (Snd y) = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Fst a,
         return $ Snd b]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

-- 9.

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) =>  Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine ( \n ->  f(n) <> g(n) )


--import Test.QuickCheck
--newtype Combine a b = Combine { unCombine :: (a -> b)}
--instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--    arbitrary = do
--        f <- arbitrary
--        return $ Combine f


-- 10. 

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

--instance (CoArbitrary a) => Arbitrary (Comp a) where
--    arbitrary = do
--        f <- arbitrary
--        return $ Comp f


-- 11. 

data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

-- everything in failure is mappendable - only failures are mappendable
-- acummulate left - so we choose the left type when possible

instance (Semigroup a) => Semigroup (Validation a b) where
  (Failure' x) <> (Failure' y) = Failure' (x <> y)
  (Failure' x) <> (Success' _) = Failure' x
  (Success' _) <> (Failure' y) = Failure' y
  (Success' _) <> (Success' y) = Success' y
  

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = validationGen

validationGen :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
validationGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Failure' a,
         return $ Success' b]

type ValidationAssoc = Validation String Ordering -> Validation String Ordering -> Validation String Ordering -> Bool

-- 12.
-- Reread 12.3

-- opposite accum left - you'll need to wrap and unwrap it.
newtype AccumulateRight a b =
  AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success' x)) <> (AccumulateRight (Success' y)) = AccumulateRight . Success' $ (x <> y)
  (AccumulateRight (Success' x)) <> (AccumulateRight (Failure' _)) = AccumulateRight (Success' x)
  (AccumulateRight (Failure' _)) <> (AccumulateRight (Success' y)) = AccumulateRight (Success' y)
  (AccumulateRight (Failure' _)) <> (AccumulateRight (Failure' y)) = AccumulateRight (Failure' y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
      a <- arbitrary
      return (AccumulateRight a)

--oneof [fmap (AccumulateRight . Failure') a,
--       fmap (AccumulateRight . Success') b]

type AccumulateRightAssoc = AccumulateRight String Ordering -> AccumulateRight String Ordering -> AccumulateRight String Ordering -> Bool

-- 13.
newtype AccumulateBoth a b = 
  AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Success' x)) <> (AccumulateBoth (Success' y)) = AccumulateBoth . Success' $ (x <> y)
  (AccumulateBoth (Success' x)) <> (AccumulateBoth (Failure' y)) = AccumulateBoth (Failure' y)
  (AccumulateBoth (Failure' x)) <> (AccumulateBoth (Success' y)) = AccumulateBoth (Failure' x)
  (AccumulateBoth (Failure' x)) <> (AccumulateBoth (Failure' y)) = AccumulateBoth . Failure' $ (x <> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
      a <- arbitrary
      return (AccumulateBoth a)

type AccumulateBothAssoc = AccumulateBoth String Ordering -> AccumulateBoth String Ordering -> AccumulateBoth String Ordering -> Bool




