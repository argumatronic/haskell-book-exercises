myWords :: [Char] -> [[Char]]
myWords [] = []
myWords xs = case az of
                      [] -> []
                      az -> az : myWords (dropWhile (/= ' ') wsz)
               where wsz = (dropWhile (== ' ') xs)
                     az  = (takeWhile (/= ' ') wsz)