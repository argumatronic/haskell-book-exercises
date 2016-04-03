myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pr = foldr f []
  where f a b
          | pr a = a : b
          | otherwise = b
