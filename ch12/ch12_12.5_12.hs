rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Left a) xs = xs
        f (Right b) xs = b : xs

-- rights' [Left 1, Left 2]
-- []
-- rights' [Right 0, Left 5, Right 4]
-- [0, 4]
