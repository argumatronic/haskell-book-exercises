countVowels :: String -> Int
countVowels = length . wordVowels

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiou"

wordVowels :: String -> String
wordVowels = filter isVowel
