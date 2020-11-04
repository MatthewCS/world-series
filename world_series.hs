{-
    A solution to the question "How many different ways can
    the world series go?" Uses the L.A. Dodgers and the
    Tampa Bay Rays as example teams.
    
    I wanted to write this code as an example problem in
    Haskell. I'll probably try solving this in other
    languages as well.
    
    Call the functions PossibilitiesTotal or
    PossibilitiesExact in GHCI to get the numbers. If you
    want to see each series itself, call worldseries on an
    empty list.
    
    Example output:
        *Main> filter (\x -> length x == 4) (worldseries [])
        [[Dodgers,Dodgers,Dodgers,Dodgers],[Rays,Rays,Rays,Rays]]
        *Main> possiblitiesExact 7
        40
        *Main> possiblitiesTotal 7
        70
-}

data Team = Dodgers | Rays
  deriving (Show, Eq)


-- Combinations of paths to win the world series
worldseries :: [Team] -> [[Team]]
worldseries []
  = (worldseries [Dodgers] ++ worldseries [Rays])
worldseries series@(t:_)
  | count t series == 4
                    = [reverse series]
  | otherwise       = (worldseries (t:series)) ++ (worldseries (opponent:series))
    where opponent = if t == Dodgers then Rays else Dodgers


-- Count the number of occurances of some element in a list
count :: Eq a => a -> [a] -> Int
count _ []
  = 0
count e (x:xs)
  | e == x          = 1 + (count e xs)
  | otherwise       = count e xs


-- How many combinations of winners can we have in a series
-- of N games or less?
possiblitiesTotal :: Int -> Int
possiblitiesTotal n
  = length $ filter (\x -> length x <= n) (worldseries [])


-- How many combinations of winners can we have in a series
-- of N total games?
possiblitiesExact :: Int -> Int
possiblitiesExact n
  = length $ filter (\x -> length x == n) (worldseries [])