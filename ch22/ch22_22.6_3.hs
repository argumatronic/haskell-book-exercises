-- you'll need this pragma
{-# LANGUAGE InstanceSigs #-}

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b

  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)