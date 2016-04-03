import Data.Char (toUpper, isAlpha)
import Data.List (groupBy)
import Data.Function (on)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs)
    | isAlpha x = toUpper x : xs
    | otherwise = x : capitalizeWord xs

capitalizeParagraph :: String -> String
capitalizeParagraph xs = concatMap capitalizeWord $ groupBy ((==) `on` (=='.')) xs
