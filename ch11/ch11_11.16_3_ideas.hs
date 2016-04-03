import Data.Char (toUpper, isAsciiLower)
import Data.List (groupBy)
import Data.Function (on)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs)
  | isAsciiLower x = toUpper x : xs
  | otherwise = x : capitalizeWord xs


capitalizeParagraph :: String -> String
capitalizeParagraph xs = concatMap capSentence $ groupBy ((==) `on` (=='.')) xs

capSentence :: String -> String
capSentence [] = ""
capSentence ax@(x:xs) = if isAsciiLower x then capitalizeWord ax else x : capSentence xs



capitalizeParagraph :: String -> String
capitalizeParagraph xs = foldr cap (True, []) xs where
  cap (True, xs) '.' = (True, )
  cap (True, xs) y   = if isAsciiLower x then (False) else x : capSentence xs

capitalizeParagraph :: String -> String
capitalizeParagraph (x:xs) = cap True xs where
  cap _ ('.':xs) = '.':

capSentence :: String -> String
capSentence [] = ""
capSentence ax@(x:xs) = if isAsciiLower x then capitalizeWord ax else x : capSentence xs


words   :: String -> [String]
words s =  case break ((/=) '.') s of
                      (a,b) -> 
                      s' -> w : words s''
                            where (w, s'') = break Char.isSpace s'

breakAfter _ xs@[]           =  (xs, xs)
breakAfter p xs@(x:xs')
             | p x        =  ([x],xs')
             | otherwise  =  let (ys,zs) = breakAfter p xs' in (x:ys,zs)