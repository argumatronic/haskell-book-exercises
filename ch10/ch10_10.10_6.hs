myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\a b -> f a : b) [] xs

-- answer from the book is nicer
myMap' f = foldr ((:) . f) []