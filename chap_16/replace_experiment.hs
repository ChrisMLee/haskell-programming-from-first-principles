module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe[Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe[Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f(f1 a) -> f(f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

twiceLifted'' :: (Functor f) => f(Maybe [Char]) -> f(Maybe Char)
twiceLifted'' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

thriceLifted'' :: (Functor f) => f(Maybe[Char]) -> f(Maybe [Char])
thriceLifted'' = thriceLifted

thriceLifted''' :: (Functor f, Functor f1) => f(Maybe (f1(Char))) -> f(Maybe (f1(Char)))
thriceLifted''' = thriceLifted

thriceLifted'''' :: (Functor f, Functor f1) => f(f1([Char])) -> f(f1([Char]))
thriceLifted'''' = thriceLifted

main :: IO ()
main = do
  putStr "replaceWithP' lms:   "
  print (replaceWithP' lms)

  putStr "liftedReplace lms:   "
  print (liftedReplace lms)

  putStr "liftedReplace' lms:  "
  print (liftedReplace' lms)
  putStr "twiceLifted lms:     "
  print (twiceLifted lms)
  putStr "twiceLifted' lms:    "
  print (twiceLifted' lms)
  putStr "twiceLifted'' lms:   "
  print (twiceLifted'' lms)
  putStr "thriceLifted lms:    "
  print (thriceLifted lms)
  putStr "thriceLifted' lms:   "
  print (thriceLifted' lms)
  putStr "thriceLifted'' lms:   "
  print (thriceLifted'' lms)
  putStr "thriceLifted''' lms:   "
  print (thriceLifted''' lms)
  putStr "thriceLifted''''  lms:   "
  print (thriceLifted'''' lms)

