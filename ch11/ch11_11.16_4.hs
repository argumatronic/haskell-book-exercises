{-|
 ---------------------------
 | 1      | 2 ABC | 3 DEF  |
 ___________________________
 | 4 GHI  | 5 JKL | 6 MNO  |
 ---------------------------
 | 7 PQRS | 8 TUV | 9 WXYZ |
 ---------------------------
 | * ^    | 0 + _ | # .,   |
 ---------------------------
-}

import Data.List (maximumBy)

data DaPhone = Sommat

convo :: [String]
convo = ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- valid presses = [1..4]
type Presses = Int


cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

popularest :: String -> Char
popularest = snd maximumBy fst . frequency

-- from SO
frequency :: Ord a => [a] -> [(Int,a)] 
frequency list = map (\l -> (length l, head l)) (group (sort list))

reverseTaps :: Char -> (Digit, Presses)
reverseTaps = undefined
-- reverseTaps 'a' == [('2', 1)]
-- reverseTaps 'A' == [('*', 1), ('2', 1)]

coolestLtr :: [String] -> Char
coolestLtr = snd . maximumBy fst . maximumBy fst . map frequency 

coolestWord :: [String] -> String
coolestWord = snd . maximumBy fst . map frequency . map words 
