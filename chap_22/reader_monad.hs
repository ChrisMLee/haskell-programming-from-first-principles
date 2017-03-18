{-# LANGUAGE InstanceSigs #-}

module ReaderMonad where

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (fmap f ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab(r) <$> ra)(r)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> (runReader $ aRb (ra r))(r)


ask :: Reader a a
ask = Reader id


-----------

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String 
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = 
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = 
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

-- Rewrite the monadic getDogRM to use your Reader datatype

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address 
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = do
  name <- ask
  addy <- ask 
  return $ Dog (dogName name) (address addy)

main :: IO()
main = do
  print $ getDogRM chris
  print $ runReader getDogRM' chris

-- runReader getDogRM' chris





