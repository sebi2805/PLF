listDoubleOddLTn :: [Int] -> Int -> [Int]
listDoubleOddLTn [] _ = []
listDoubleOddLTn (x:xs) n  
    | odd x && x < n = (*) x 2 : listDoubleOddLTn xs n
    | otherwise = listDoubleOddLTn xs n
    

listDoubleOddLTn' :: [Int] -> Int -> [Int]
listDoubleOddLTn' ls n = [(*) x 2 | x<- ls, odd x && x<n]


listDoubleOddLTn'' :: [Int] -> Int -> [Int]
listDoubleOddLTn'' sir n = map (\x -> (*) x 2) $ filter (\ls -> odd ls && ls<n ) sir 



isVowel :: Char -> Bool
isVowel ch = elem ch "aeiouAEIOU"

onlyVowelsAux :: String -> String
onlyVowelsAux ls = [ ch | ch <-ls, isVowel ch]

onlyVowels :: [String] -> [String]
onlyVowels lss = [onlyVowelsAux ls | ls <- lss, not((length (onlyVowelsAux ls)) == 0)]