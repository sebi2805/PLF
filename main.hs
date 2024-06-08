hello_world = putStrLn  "Hello World"

paritate :: Int -> Bool
paritate x = if x `mod` 2 == 0 then True else False

min' :: [Int] -> Int 
min' (h:t) = minAcc t h 
          where 
          minAcc [] minV = minV 
          minAcc (h:t) minV = if h < minV then minAcc t h else minAcc t minV


numerePare :: [Int] -> [Int]
numerePare list = map (^2) $ filter paritate list


fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fiboList :: Int -> [Int]
fiboList n = take n fiboSeq
  where
    fiboSeq = 0 : 1 : zipWith (+) fiboSeq (tail fiboSeq)

main = print (fiboList 10) 