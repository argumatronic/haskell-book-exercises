import Data.Char (toUpper)

fstCapCom :: [Char] -> Char
fstCapCom xs = (toUpper . head) xs

fstCapPf :: [Char] -> Char
fstCapPf = toUpper . head
