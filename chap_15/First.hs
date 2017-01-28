module First where

import Data.Monoid
import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (mappend x y)
  mappend Nada (Only x) = (Only x)
  mappend (Only x) Nada = (Only x)


newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only a)) _ = First' (Only a)
  mappend (First' Nada) b     = b

firstMappend :: First' a ->
                First' a ->
                First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

-- First' (Only (Sum 2))

-- data Tree a = Node a (Forest a)

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

--instance Arbitrary a => Arbitrary (First' a) where
--  arbitrary = oneof [ (return $ First' Nada)
--                    , (fmap First' (fmap Only arbitrary)) ]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = oneof [ (return $ First' Nada)
                    , (fmap (First' . Only) arbitrary) ]


--f :: [a] -> [First' a]

--firstGen:: Arbitrary a => Gen (First' a)
--firstGen = 
--a <- arbitrary
--oneof [ (First' Nada)
--      , (fmap (First' . Only) arbitrary) ]


--instance Arbitrary a => Arbitrary (First' a) where
--  arbitrary = do
--     a <- arbitrary
--     return (First' a)

--newtype Sum a = Sum {getSum :: a}








