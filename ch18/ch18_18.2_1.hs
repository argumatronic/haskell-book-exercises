import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

-- bind (\ x -> [x,1]) [4,5,6]