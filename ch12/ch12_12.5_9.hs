catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just a:xs) = a : catMaybes xs

-- using fold
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' xs = foldr f [] xs
  where f Nothing xs'  = xs'
        f (Just a) xs' = a : xs'