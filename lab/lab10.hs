ordered ::[a] -> (a -> a -> Bool) -> Bool
ordered [] _ = True
ordered [x] _ = True
ordered (x:y:xs) cmp = cmp x y && ordered (y:xs) cmp

natOrdered :: [Int] -> Bool
natOrdered list = ordered list (<=)

-- data Line = L [Int] deriving (Show)
-- data Matrix = M [Line] deriving (Show)

-- linesN :: Matrix -> Int -> [Line]
-- linesN (M []) _ = []
-- linesN (M (x:xs)) n = if length x == n
--                       then x : linesN (M xs) n
--                       else linesN (M xs) n



-- allPositive :: [Int] -> Bool
-- allPositive = all (> 0)


-- onlyPosElems :: Matrix -> Int -> Bool
-- onlyPosElems m n = all (allPositive . getLineElems) (linesN m n)
--   where
--     getLineElems (L lst) = lst

isVowel :: Char -> Maybe Char
isVowel c
  | c `elem` "aeiouAEIOU" = Just c
  | otherwise = Nothing

putStrLnVowel :: String -> IO ()
putStrLnVowel [] = return ()
putStrLnVowel (x:xs) = case isVowel x of
  Nothing -> putChar x >> putStrLnVowel xs
  Just v -> putChar x >> putChar 'p' >> putChar v >> putStrLnVowel xs




data Expr = Const Int
        | Expr :+: Expr
        | Expr :*: Expr
        deriving Eq

data Operation = Add | Mult deriving (Eq, Show)
data Tree = Lf Int
        | Node Operation Tree Tree
        deriving (Eq, Show)


instance Show Expr where
        show (Const x) = show x 
        show (expr1 :+: expr2) = "(" ++ show expr1 ++ "+" ++ show expr2++ ")"
        show (expr1 :*: expr2) = "(" ++ show expr1 ++ "*" ++ show expr2++ ")"

evalExp :: Expr -> Int
evalExp (Const expr) = expr 
evalExp (expr1 :+: expr2) = evalExp expr1  + evalExp expr2 
evalExp (expr1 :*: expr2) = evalExp expr1  * evalExp expr2 


evalArb :: Tree -> Int
evalArb (Lf leaf) = leaf 
evalArb (Node Add left right) =  (+) (evalArb left) (evalArb right) 
evalArb (Node Mult left right) =  (*) (evalArb left) (evalArb right) 