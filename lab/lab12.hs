(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
f <=< g = (\ x -> g x >>= f)
asoc :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Bool
asoc f g h x = undefined
pos :: Int -> Bool
pos x = if (x>=0) then True else False
foo :: Maybe Int -> Maybe Bool
foo mx = mx >>= (\x -> Just (pos x))
addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = undefined

