myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f x xs
  where go f b (x:xs)
          | f b x == GT = go f b xs
          | otherwise = go f x xs
        go f b [] = b

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f x xs
  where go f b (x:xs)
          | f b x == LT = go f b xs
          | otherwise = go f x xs
        go f b [] = b

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy (\a b -> compare a b) xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy (\a b -> compare a b) xs
