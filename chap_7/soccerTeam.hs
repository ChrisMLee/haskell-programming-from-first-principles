module SoccerTeam where
  data Player = Goalie
              | Defender
              | Midfielder
              | Winger
              | Striker
              deriving (Eq, Ord, Show)

  reportPosition:: Player -> Player -> IO()
  reportPosition p p' =
    putStrLn $ show p ++ " is in front of " ++ show p'

  goaliesAreCool:: Player -> Player -> Ordering
  goaliesAreCool Goalie Goalie = EQ
  goaliesAreCool Goalie _ = GT
  goaliesAreCool _ Goalie = LT
  goaliesAreCool p p' = compare p p'


  fieldPosition:: (Player -> Player -> Ordering)
                  -> Player
                  -> Player
                  -> IO()
  fieldPosition f p p' =
    case f p p' of
      GT -> reportPosition p p'
      EQ -> putStrLn "Both players are in the same area of the field"
      LT -> (flip reportPosition) p p'

--We were able to rely on the behavior of compare but make changes in the part we wanted to change.
--This is the value of HOFs. They give us the beginnings of a powerful method for reusing and composing code.
