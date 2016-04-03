myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []     = undefined
myMinimumBy _ [x]    = x
myMinimumBy f (x:xs) = foldl fo x xs
  where fo a b
          | f a b == LT = a
          | otherwise = b
