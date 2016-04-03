myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = if a == x then True else myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' a (x:xs)
    | a == x = True
    | otherwise = myElem a xs

myElem'' :: Eq a => a -> [a] -> Bool
myElem'' _ [] = False
myElem'' a xs = any (\x -> a == x) xs

myElem''' :: Eq a => a -> [a] -> Bool
myElem''' _ [] = False
myElem''' a xs = any (a ==) xs
