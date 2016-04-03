isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] ys = True
isSubsequenceOf _ []  = False
isSubsequenceOf ax@(x:xs) (y:ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf ax ys
