data DividedResult =
      Result Integer
    | DividedByZero deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom
    | denom == 0 = DividedByZero
    | otherwise = go (abs num) (abs denom) 0 ((signum num) * (signum denom))
    where go n d count s
            | n < d = Result (s * count)
            | otherwise = go (n - d) d (count + 1) s
