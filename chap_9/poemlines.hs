module PoemLines where
  firstSen = "Tyger Tyger, burning bright\n"
  secondSen = "In the forests of the night\n"
  thirdSen = "What immortal hand or eye\n"
  fourthSen = "Could frame thy fearful symmetry?"
  sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

  -- putStrLn sentences -- should print
  -- Tyger Tyger, burning bright
  -- In the forests of the night
  -- What immortal hand or eye
  -- Could frame thy fearful symmetry?
  -- Implement this

  myLines :: String -> Char -> [String] 
  myLines x p =  reverse (go x []) where
    go x' xs = case (dropWhile(== p) x') of
                  "" -> xs
                  anythingyouwant -> go ((dropWhile (/= p) anythingyouwant)) ((takeWhile (/= p) anythingyouwant) : xs)
  --"Tyger Tyger, burning bright\nIn the forests of the night\nWhat immortal hand or eye\nCould frame thy fearful symmetry?"

  -- What we want 'myLines sentences' to equal
  shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

  -- The main function here is a small test -- to ensure you've written your function -- correctly.
  main :: IO ()
  main =
    print $ "Are they equal? "
          ++ show (myLines sentences '\n' == shouldEqual)