import Data.Char
import Control.Applicative (liftA2)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) cap rev

tupled_do :: [Char] -> ([Char], [Char])
tupled_do = do
  a <- rev
  b <- cap
  return (a, b)

tupled_bind :: [Char] -> ([Char], [Char])
tupled_bind = rev >>= \x1 -> cap >>= \x2 -> return (x1, x2)