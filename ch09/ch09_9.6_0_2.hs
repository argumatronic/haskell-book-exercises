myWords :: [Char] -> [[Char]]
myWords [] = []
myWords a@(x:xs)
    | x == ' ' = myWords xs
    | otherwise = (takeWhile (/= ' ') a) : myWords (dropWhile (/= ' ') a)