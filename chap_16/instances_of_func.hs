module InstancesOfFunc where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
  quickCheck (functorCompose' :: IdentityFC)
  quickCheck $ \x -> functorIdentity (x :: (Identity Int))
  quickCheck (functorCompose' :: PairFC)
  quickCheck $ \x -> functorIdentity (x :: (Pair Int))
  quickCheck (functorCompose' :: TwoFC)
  quickCheck $ \x -> functorIdentity (x :: (Two Int Int))
  quickCheck (functorCompose' :: ThreeFC)
  quickCheck $ \x -> functorIdentity (x :: (Three Int Int Int))
  quickCheck (functorCompose' :: Three'FC)
  quickCheck $ \x -> functorIdentity (x :: (Three' Int Int))
  quickCheck (functorCompose' :: FourFC)
  quickCheck $ \x -> functorIdentity (x :: (Four Int Int Int Int))
  quickCheck (functorCompose' :: Four'FC)
  quickCheck $ \x -> functorIdentity (x :: (Four' Int Int))


--1.

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary 
    return (Identity a)

type IdentityFC = (Identity Int) -> IntToInt -> IntToInt -> Bool

--2. 

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a ) (f b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary 
    return (Pair a a)

type PairFC = (Pair Int) -> IntToInt -> IntToInt -> Bool

--3. 

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b)=> Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary 
    return (Two a b)

type TwoFC = (Two Int Int) -> IntToInt -> IntToInt -> Bool

--4. 

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary 
    c <- arbitrary 
    return (Three a b c)

type ThreeFC = (Three Int Int Int) -> IntToInt -> IntToInt -> Bool

--5. 

data Three' a b = Three' a b b deriving (Eq, Show)

-- Type needs have kind * -> *
instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary 
    return (Three' a b b)

type Three'FC = (Three' Int Int) -> IntToInt -> IntToInt -> Bool

--6. 
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) =  Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary 
    c <- arbitrary
    d <- arbitrary 
    return (Four a b c d)

type FourFC = (Four Int Int Int Int) -> IntToInt -> IntToInt -> Bool

--7. 
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary 
    return (Four' a a a b)

type Four'FC = (Four' Int Int) -> IntToInt -> IntToInt -> Bool

--8. 

-- Not possible - this is not higher kinded e.g. * -> *
data Trivial = Trivial
























