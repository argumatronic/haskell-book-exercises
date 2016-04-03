{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, n') =  tooMany (n + n')
