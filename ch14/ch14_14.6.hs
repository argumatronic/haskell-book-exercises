listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


broken = listOrdered [2,3,6,2,56,3,4,5]


listOrderedFixed :: (Ord a) => [a] -> Bool
listOrderedFixed xs = snd $ foldr go (Nothing, True) xs
  where go y (_, False) = (Nothing, False)
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

notsorted = listOrderedFixed [2,3,6,2,56,3,4,5]
sorted = listOrderedFixed [2,3,6,56]