module GimmePerson where
import Control.Monad (forever)
import System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String 
                   deriving (Eq, Show)

mkPerson :: Name
            -> Age
            -> Either PersonInvalid Person 
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = forever $ do
  hSetBuffering stdout NoBuffering
  putStr "Please enter a name: "
  name <- getLine
  putStr "Please enter an age: "
  age <- getLine
  case (mkPerson name (read age :: Age)) of
    (Right (Person name age)) -> putStrLn $ "Yay! Successfully got a person: " ++  "Name- " ++ (name) ++ ". Age- " ++ (show age)
    (Left NameEmpty) -> putStrLn "Error: NameEmpty"


--putStrLn $ "encrypted text: " ++ runCipher str (read shift :: Int)