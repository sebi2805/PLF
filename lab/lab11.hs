import Data.Char (isDigit, digitToInt)


-- exercitiul 1
-- sa se calculeze produsul elementelor pare de pe pozitii de 5 
-- high order function 
product' :: [Int] -> Int
product' list = foldr (\(x, y) acc -> if mod x 5 == 0 && mod y 2 == 0 then acc * x else acc ) 1 $ zip list [0..]

-- recursiv 
-- functie auxiliara
isValid :: Int -> Int -> Bool
isValid num idx 
    | mod num 5 == 0 && mod idx 2 == 0 = True
    | otherwise = False

product'' :: [Int] -> Int
product'' list = product''' list 0

product''' :: [Int] -> Int -> Int
product''' [] _ = 1
product''' (x:xs) n 
    | isValid x n = x * (product''' xs $ n+1)
    | otherwise = product''' xs $ n+1

-- exercitiul 2 
-- o functie care primeste un string si utilizand foldr din proprietatea de universalitate
-- determina suma cifrelor pare care apar in sir 
-- import Data.Char isDigit, digitToInt
charSum :: String -> Int 
charSum list  = foldr (\(x) acc -> if isDigit x && (mod (digitToInt x) 2 == 0)  then acc + (digitToInt x) else acc ) 0 list 

-- exercitiul 3 
-- fie tipul de date algebric
data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a)
-- scrieti instante pentru pentru eq, show, semigrup, monoid, foldable, functor

instance Eq a => Eq (BinaryTree a) where 
    Nil == Nil = True 
    Node a l1 r1 == Node b l2 r2 = a == b && (l1 == l2) && (r1 == r2)
    _ == _ = False 

instance Show a => Show (BinaryTree a) where
    show Nil = "Nil" 
    show (Node a l r) = "(" ++ show a ++ " (" ++ show l ++ ") " ++ " (" ++ show r ++ ") " ++ ")"


instance Monoid a => 