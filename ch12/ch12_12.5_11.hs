lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left a) xs = a : xs
        f (Right b) xs = xs

-- lefts' [Left 1, Left 2]
-- [1, 2]
-- lefts' [Right 0, Left 5, Right 4]
-- [5]
