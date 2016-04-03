import Data.Char

vigenere :: String -> String -> String
vigenere xs ys = vigenere' xs (cycle ys)

vigenere' [] _ = ""
vigenere' (' ':xs) cyp        = ' ' : vigenere' xs cyp
vigenere' (x:xs)   cyp@(y:ys) = docyp x y : vigenere' xs ys
  where base      = ord 'A'
        r         = 26
        dist c    = ord c - base
        docyp x y = chr $ (dist x + dist y) `mod` r + base

main = print $ vigenere "MEET AT DAWN" "ALLY" == "MPPR AE OYWY"