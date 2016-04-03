isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] ys = True
isSubsequenceOf _ []  = False
isSubsequenceOf xs ys = issub xs xs ys where
    issub _   (x:[])    (y:ys) = x == y
    issub _   (x:xs)    (y:[]) = False
    issub xs' (x:xs) ay@(y:ys)
        | x == y    = issub xs' xs ys
        | otherwise = issub xs' xs' ys
