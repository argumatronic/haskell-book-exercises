myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []     = undefined
myMaximumBy _ [x]    = x
myMaximumBy f (x:xs) = foldl fo x xs
  where fo a b
          | f a b == GT = a
          | otherwise = b
