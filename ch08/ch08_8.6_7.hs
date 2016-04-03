module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n =
    case n of
        0 -> "zero"
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"

digits :: Int -> [Int]
digits n = go n []
    where go a xs 
            | a > 9 = go (div a 10) ((mod a 10):xs)
            | otherwise = a:xs
            where 

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits