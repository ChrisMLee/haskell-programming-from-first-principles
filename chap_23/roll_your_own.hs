module RollYourOwn where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import Data.Maybe

-- Six-sided die
data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use this tactic _extremely_ sparingly.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x


rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= (fromJust $ Just n) = count
          | otherwise = 
            let (die, nextGen) = randomR (1,6) gen
            in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextGen) = randomR (1,6) gen
            in go (sum + die) (fst count + 1, ((intToDie die) : snd count)) nextGen


