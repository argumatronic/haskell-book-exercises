import Data.Char

vigenere :: String -> String -> String
vigenere xs ys = vigenere' xs (cycle ys)

vigenere' :: String -> String -> String
vigenere' [] _ = ""
vigenere' xs [] = xs
vigenere' (x:xs) cyp@(y:ys)
  | x == ' ' = x : vigenere' xs cyp
  | otherwise = docyp x y : vigenere' xs ys
  where base = ord 'A'
        r = 26
        dist c = ord c - base
        docyp a b = chr $ (dist a + dist b) `mod` r + base

main :: IO ()
main = do
  putStr "input vigenere cypher: "
  cyp <- getLine
  putStr "input text: "
  str <- getLine
  putStrLn $ "encrypted text: " ++ vigenere str cyp
