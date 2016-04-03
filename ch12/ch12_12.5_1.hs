countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = f 0 $ words s

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiou"

f :: Integer -> [String] -> Integer
f n (x:y:ys)
  | x == "the" && isVowel (head y) = f (n + 1) (y:ys)
  | otherwise = f n ys
f n _ = n
