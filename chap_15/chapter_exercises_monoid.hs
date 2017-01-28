module ChapterExercisesMonoid where
import Test.QuickCheck
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid, Sum(Sum, getSum))

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  print $ unCombine (mappend f mempty) $ 1
  print $ unComp (mappend g h) $ 1
  print $ unComp (mappend h mempty) $ 1
  print $ unComp (Comp $ \ (Sum x) -> (Sum x - 1)) $ 1

-- 1. 
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
   arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

--2. 

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity x) (Identity y) = Identity (mappend x y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3.

data Two a b = Two a b deriving (Eq, Show)

-- Semigroup provides the associative function
instance (Semigroup a, Semigroup b) =>  Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

-- Monoid provides the identity function
instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

--4.

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj (choose(True,False)) 

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


--5. 
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj (if elem True [a,b] then True else False)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>) 

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj (choose(True, False))

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

--6.
newtype Combine a b = 
  Combine {unCombine :: (a -> b)}

instance (Semigroup b) =>  Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine ( \n ->  f(n) <> g(n) )

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)  

f = Combine $ \n -> Sum (n + 1)

-- 7.
newtype Comp a =
  Comp {unComp :: (a -> a)}

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

instance (Monoid b, Semigroup b) => Monoid (Comp b) where
  mempty = Comp id
  mappend = (<>)


g = Comp $ \ (Sum x) -> (Sum x - 1)

h = Comp $ \ (Sum x) -> (Sum x + 1)

-- 8. 

newtype Mem s a =
  Mem { runMem :: s -> (a,s)}

instance (Semigroup a) => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem (\n -> (\(s, a) -> ((fst (f $ a) <> s), snd (f $ a)) ) $ g(n))

instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty,s))
  mappend = (<>)


f' = Mem $ \s -> ("hi", s + 1)

-- Prelude> mempty :: String
-- ""

mainTwo = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0


--(\n -> (\(s, a) -> y $ a ) $ z(n)) $ 0

--(\n -> (\(s, a) -> ((fst $ (y $ a)) <> s, (snd $ (y $ a)) ) ) $ z(n)) $ 0

