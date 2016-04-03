data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn b) = a == b
