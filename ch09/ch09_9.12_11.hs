myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) = go f x xs
  where go f b (x:xs)
          | f b x == GT = go f b xs
          | otherwise = go f x xs
        go f b [] = b
