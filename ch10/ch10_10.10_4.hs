myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (\ x y -> (a == x) || y) False
