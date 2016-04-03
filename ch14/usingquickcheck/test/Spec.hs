import Test.QuickCheck
-- import Test.QuickCheck.Instances.List (anyList)
import Test.QuickCheck.Modifiers (NonZero)

import Data.List (sort)

-- 1.
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

propHalf :: Float -> Bool
propHalf x = halfIdentity x == x

-- 2.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

propSort :: [Int] -> Bool
propSort = listOrdered . sort

-- 3.
plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
  x + y == y + x

-- 4.
multiAssociative :: Int -> Int -> Int -> Bool
multiAssociative x y z =
  x * (y * z) == (x * y) * z

multiCommutative :: Int -> Int -> Bool
multiCommutative x y =
  x * y == y * x

-- 5.
propQuotRem :: NonZero Int -> NonZero Int -> Bool
propQuotRem (NonZero x) (NonZero y) =
  (quot x y)*y + (rem x y) == x

propDivMod :: NonZero Int -> NonZero Int -> Bool
propDivMod (NonZero x) (NonZero y) =
  (div x y)*y + (mod x y) == x

-- 6.
powerAssociative :: Int -> Int -> Int -> Bool
powerAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

powerCommutative :: Int -> Int -> Bool
powerCommutative x y = x ^ y == y ^ x

-- 7.
propReverse :: [Int] -> Bool
propReverse xs = (reverse . reverse) xs == id xs

-- 8.
applyProp = (f $ a) == (f a)

composeProp = f . g == \x -> f (g x)

-- foldr (:) == (++)
-- foldr (++) [] == concat

-- f n xs = length (take n xs) == n

-- f x = (read (show x)) == x

main :: IO ()
main = do
  quickCheck propHalf
  quickCheck propSort
  quickCheck plusAssociative
  quickCheck plusCommutative
  quickCheck multiAssociative
  quickCheck multiCommutative
  quickCheck propQuotRem
  quickCheck propDivMod
  quickCheck powerAssociative
  quickCheck powerCommutative
  quickCheck propReverse
