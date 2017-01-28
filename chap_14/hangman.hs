import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec $ do
        describe "Addition" $ do
          it "given 5 returns 15" $ do
            sumAll 5 `shouldBe` 15

data Puzzle = Puzzle String [Maybe Char] [Char]
-- String: the word we’re trying to guess
-- [Maybe Char]: the characters we’ve filled in so far
-- [Char]: the letters we’ve guessed so far

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word
                 filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          let zd = (zipper c)
          in zipWith zd word filledInSoFar

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'