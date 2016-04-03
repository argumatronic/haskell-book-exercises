foldBool :: a -> a -> Bool -> a
foldBool x y z
    | z = x
    | otherwise = y