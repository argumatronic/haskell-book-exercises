myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs)
    | x = x
    | otherwise = myOr xs
