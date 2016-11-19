module SquareCube where
  mySqr = [x^2 | x <- [1..5]]
  myCube = [y^3 | y <- [1..5]]

  constrained = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
  howManyTups = length constrained

  main :: IO ()
  main = do 
    print ([(x, y) | x <- mySqr, y <- myCube])
    print constrained
    print howManyTups
      