myWords :: String -> [String]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords xs =  takeWhile (/= ' ') xs:myWords (dropWhile (/= ' ') xs)