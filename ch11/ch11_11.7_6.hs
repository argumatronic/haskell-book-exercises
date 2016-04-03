{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance (Num a, Integral a, TooMany a) => TooMany (a, a) where
  tooMany (n, n') = odd (n + n')


instance (Num a, Integral a) => TooMany (a, a) where
  tooMany (n, n') = odd (n + n')


instance (Num a, Ord a, TooMany a) => TooMany (a, a) where
  tooMany (n, n') = (n + n') > 42


instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, n') = (n + n') > 42


instance Num a => TooMany (a, a) where
  tooMany (n, n') = (n + n') > 42


instance (Num a, Ord a) => TooMany (a, a) where
  tooMany (n, n') = (n + n') > 42