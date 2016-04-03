module Addition where

someFunc :: IO ()
someFunc = putStrLn "some Func"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n
          d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

recMul :: (Eq a, Num a) => a -> a -> a
recMul n m = go n m 0
    where go n' m' acc
            | m' == 0 = acc
            | otherwise = go n' (m' - 1) (acc + n')
