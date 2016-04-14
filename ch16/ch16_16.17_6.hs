{-# LANGUAGE FlexibleInstances #-}

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b =
  K a

-- should remind you of an
-- instance you've written before
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))