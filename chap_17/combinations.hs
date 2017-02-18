module Combinations where

import Control.Applicative(liftA3)

stops::String
stops = "pbtdkg"

vowels::String
vowels = "aeiou"

combos:: [a] -> [b] -> [c] -> [(a,b,c)]
combos s v s' = liftA3 (\a b c -> (a, b, c)) s v s'
-- or combos s v s' = liftA3 (,,) s v s'



--liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d