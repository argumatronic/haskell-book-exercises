data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a1 a2) (Pair b1 b2) = a1 == b1 && a2 == b2