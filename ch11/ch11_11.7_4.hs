class TooMany a where
  tooMany :: a -> Bool

newtype Goats = Goats (Int, Int) deriving Show

instance TooMany Goats where
  tooMany (Goats (n, n')) = (n + n') > 42
