partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where f (Left a) (xs, ys) = (a:xs, ys)
        f (Right b) (xs, ys) = (xs, b:ys)
