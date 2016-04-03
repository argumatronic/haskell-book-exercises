myWords :: [Char] -> [[Char]]
myWords [] = []
myWords a@(x:xs)
    | x == ' ' = myWords xs
    | otherwise = (takeWhile (\c -> c /= ' ') a) : myWords (dropWhile (\c -> c /= ' ') a)