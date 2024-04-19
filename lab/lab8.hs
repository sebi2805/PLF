suma :: Int -> Int -> Int 
suma x y = x + y

suma' :: Int -> Int -> Int -> Int 
suma' x y z = x + suma y z


maxim :: Int -> Int -> Int 
maxim x y = if x > y then x else y
maxim' :: Int -> Int -> Int -> Int 
maxim' x y z = maxim x (maxim y z) 
maxim'' :: Int -> Int -> Int -> Int -> Int
maxim'' x y z w = maxim x (maxim y (maxim z w))
maxim''' :: Int -> Int -> Int -> Int -> Int -> Int
maxim''' x y z w v = maxim x (maxim y (maxim z (maxim w v)))

data Choice = 
    Rock
    | Paper
    | Scissors
    deriving (Eq, Show)

data Result = 
    Victory
    | Defeat 
    | Draw
    deriving (Eq, Show)

game :: Choice -> Choice -> Result
game x y = 
        if x == Rock then 
            if y == Paper then Defeat
            else if y == Scissors then Victory
            else Draw 
        else if x == Paper then
            if y == Paper then Draw 
            else if y == Rock then Victory 
            else Defeat 
        else if y == Scissors then Draw
            else if y == Paper then Victory
            else Defeat