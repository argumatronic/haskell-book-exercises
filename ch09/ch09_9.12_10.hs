squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\x -> x) xs

squishAgain' :: [[a]] -> [a]
squishAgain' xs = squishMap id xs