data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a1 a2) (Two b1 b2) = a1 == b1 && a2 == b2