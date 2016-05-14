import Test.QuickCheck (quickCheck, verboseCheck)
import Data.Char (toUpper, toLower)
import Data.List (sort)


twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord w
  | null w = w
  | otherwise = [toUpper firstLetter] ++ map toLower others
  where ([firstLetter], others) = splitAt 1 w

propCapitalize :: String -> Bool
propCapitalize x =
  (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

propSort :: [Int] -> Bool
propSort x =
  (sort x == twice sort x) && (sort x == fourTimes sort x)


main = do
  quickCheck propCapitalize
  quickCheck propSort
