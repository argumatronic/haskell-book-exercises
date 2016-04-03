myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                Nothing -> []
                Just (x, y) -> x : myUnfoldr f y
