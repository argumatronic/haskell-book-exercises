myWords :: [Char] -> [[Char]]
myWords [] = []
myWords xs = case dropWhile (== ' ') xs of
                      [] -> []
                      az -> w : myWords (dropWhile (/= ' ') az)
                            where w = takeWhile (/= ' ') az