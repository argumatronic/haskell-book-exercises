separate :: [Char] -> [[Char]]
separate [] = []
separate sep xs = case dropWhile (== sep) xs of
                      [] -> []
                      az -> w : myWords rest
                            where (w, rest) = break (== sep) az

myWords :: [Char] -> [[Char]]
myWords = separate ' '


myLines :: String -> [String]
myLines = separate '\n'
