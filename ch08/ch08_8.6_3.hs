recSum :: (Eq a, Num a) => a -> a
recSum n = go n 0
    where go n acc
            | n == 0 = acc
            | otherwise = go (n - 1) (acc + n)