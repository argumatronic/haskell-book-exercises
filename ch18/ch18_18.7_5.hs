meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = (++) <$> (fmap (\a -> [a]) $ f x) <*> (meh xs f)

main = do
  print $ meh [1..10] (\x -> if odd x then Just x else Just (x * 2))
