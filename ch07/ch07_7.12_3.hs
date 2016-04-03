foldBool :: a -> a -> Bool -> a
foldBool x y z =
    case z of
    True -> x
    False -> y