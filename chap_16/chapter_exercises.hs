 {-# LANGUAGE FlexibleInstances #-}
module ChapterExercises where

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

main :: IO ()
main = do
  quickCheck (functorCompose' :: QuantFC)
  quickCheck $ \x -> functorIdentity (x :: (Quant Int Int))
  quickCheck (functorCompose' :: K'FC)
  quickCheck $ \x -> functorIdentity (x :: (K' Int Int))
  quickCheck (functorCompose' :: KFC)
  quickCheck $ \x -> functorIdentity (x :: (K Int Int))

--1. 

data Quant a b = 
    Finance
  | Deska
  | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor a) = Bloor (f a)
  fmap _ Deska = Deska
  fmap _ Finance = Finance

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = quantGen

quantGen :: (Arbitrary b) => Gen (Quant a b)
quantGen = do
    b <- arbitrary 
    oneof [return $ Finance,
           return $ Deska,
           return $ Bloor b]

type QuantFC = (Quant Int Int) -> IntToInt -> IntToInt -> Bool

--2.

data K' a b = 
  K' a deriving (Eq, Show)

instance Functor (K' a) where
  fmap f (K' b) = K' b

instance (Arbitrary a) => Arbitrary (K' a b) where
  arbitrary = do
    a <- arbitrary
    return (K' a)


type K'FC = (K' Int Int) -> IntToInt -> IntToInt -> Bool

--3. 

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b =
  K a
-- should remind you of an
-- instance you've written before
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return (K a)


type KFC = (K Int Int) -> IntToInt -> IntToInt -> Bool

-- p. 661










