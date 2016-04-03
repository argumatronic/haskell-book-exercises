myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = if f x == True then True else myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _ [] = False
myAny' f (x:xs)
    | f x = True
    | otherwise = myAny f xs
