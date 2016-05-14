myLiftA2 :: Applicative f =>  (a -> b -> c) -> f a -> f b -> f c
myLiftA2 a b c = a <$> b <*> c