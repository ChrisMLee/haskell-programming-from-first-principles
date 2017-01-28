module OptionalMonoid where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (mappend x y)
  mappend Nada (Only x) = (Only x)
  mappend (Only x) Nada = (Only x)

