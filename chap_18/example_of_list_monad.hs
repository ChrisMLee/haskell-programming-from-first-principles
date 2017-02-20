module ExampleOfListMonad where


twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  [ x + 2 ]

