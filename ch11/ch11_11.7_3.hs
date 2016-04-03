class TooMany a where
  tooMany :: a -> Bool

newtype Goats = Goats (Int, String) deriving Show

instance TooMany Goats where
  tooMany (Goats (n, s)) = n > 42
