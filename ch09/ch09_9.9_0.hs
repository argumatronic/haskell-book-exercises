import Data.Bool (bool)

foldBool :: [Integer] -> [Integer]
foldBool = map (\x -> bool x (-x) (x == 3))
