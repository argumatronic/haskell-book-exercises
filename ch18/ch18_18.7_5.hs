meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (x:xs) f = fmap f xs