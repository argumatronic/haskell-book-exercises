myWords :: [Char] -> [[Char]]
myWords [] = []
myWords xs = case dropWhile (== ' ') xs of
                      [] -> []
                      az -> w : myWords rest
                            where (w, rest) = break (== ' ') az