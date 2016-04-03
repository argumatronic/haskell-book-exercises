eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

-- eitherMaybe' even (Right 2)
-- eitherMaybe' even (Left 2)
