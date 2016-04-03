flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs = foldr f (Just []) xs
  where f _ Nothing = Nothing
        f Nothing _ = Nothing
        f (Just a) (Just b) = Just (a:b)
