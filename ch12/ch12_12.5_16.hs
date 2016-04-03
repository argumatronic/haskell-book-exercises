either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl _ (Left a) = fl a
either' _ fr (Right b) = fr b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f eb = Just (either' undefined f eb)

-- eitherMaybe'' even (Right 2)
-- eitherMaybe'' even (Left 2)
