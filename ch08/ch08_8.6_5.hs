data DividedResult =
      Result Integer
    | DividedByZero deriving Show

dividedBy :: Integer -> Integer -> (DividedResult, DividedResult)
dividedBy num denom
    | denom == 0 = (DividedByZero, DividedByZero)
    | otherwise = go (abs num) (abs denom) 0 (if (num < 0) then negate else id) (if (denom < 0) then negate else id)
    where go n d count n1 n2
            | n < d = (Result (n1 . n2 $ count), Result (n2 n))
            | otherwise = go (n - d) d (count + 1) n1 n2    