myReverse :: [a] -> [a]
myReverse xs = go [] xs
  where go ys [] = ys
        go ys (x:xs) = go (x : ys) xs
