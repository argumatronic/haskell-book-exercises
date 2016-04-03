squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

-- again, answer from the book is nicer
squishMap' f = foldr ((++) . f) []