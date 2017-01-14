
module Main where
import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

newtype WordList = 
  WordList [String] 
  deriving (Eq, Show)


allWords :: IO WordList
allWords = do
        dict <- readFile "data/dict.txt"
        return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l > minWordLength && l < maxWordLength


randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]
-- String: the word we’re trying to guess
-- [Maybe Char]: the characters we’ve filled in so far
-- [Char]: the letters we’ve guessed so far

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (createNothings s) []
                  where createNothings x = map (\a -> Nothing) x

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
      (_, True) -> do
         putStrLn "You already guessed that\
                   \ character, pick something else!"
         return puzzle
      (True, _) -> do
         putStrLn "This character was in the word,\
                   \ filling in the word accordingly"
         return (fillInCharacter puzzle guess)
      (False, _) -> do
         putStrLn "This character wasn't in\
                   \ the word, try again."
         return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> [Char] -> IO ()
gameOver (Puzzle wordToGuess _ guessed) incorrect =
  if (length incorrect) > (length wordToGuess) then 
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then 
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame p@(Puzzle w f g) = forever $ do
  hSetBuffering stdout NoBuffering
  gameOver p (wrongGuesses w g)
  gameWin p
  putStrLn $ "Current puzzle is: " ++ show p
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess p c >>= runGame
    _ -> putStrLn "Your guess must\
                    \ be a single character"


wrongGuesses :: [Char] -> [Char] -> [Char]
wrongGuesses w g = foldr (\a b -> if elem a w then b else a:b) "" g





