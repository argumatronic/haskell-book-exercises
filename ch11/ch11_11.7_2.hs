{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (n, s) = n > 42
