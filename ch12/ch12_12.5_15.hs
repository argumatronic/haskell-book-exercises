either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl _ (Left a) = fl a
either' _ fr (Right b) = fr b

-- either' even odd (Left 2)
-- either' even odd (Right 2)
