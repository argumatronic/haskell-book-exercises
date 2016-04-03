import Data.Char (toUpper)

myCap :: [Char] -> [Char]
myCap (x:xs) = toUpper x : xs
myCap _ = ""
