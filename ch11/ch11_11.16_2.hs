import Data.Char (toUpper)

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map f $ words xs
  where f as@(s:st) = (as, toUpper s : st)
