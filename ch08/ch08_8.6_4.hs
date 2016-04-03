recMul :: (Integral a) => a -> a -> a
recMul n m = go n m 0
    where go n m acc
            | m == 0 = acc
            | otherwise = go n (m - 1) (acc + n)