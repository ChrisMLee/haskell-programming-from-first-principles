module ShortExercise where

data Sum' a b =
    First' a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap f (First' a) = First' a
  fmap f (Second a) = Second (f a)

-- 2. Why is a Functor instance that applies the function only to First,
-- Either’s Left, impossible? We covered this earlier.

-- Because that variable has been applied away in order to reduce the kindedness of the type to * -> *