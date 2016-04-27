import Data.Monoid
import Data.Foldable hiding (length)
import Prelude hiding (length)

length :: (Foldable t) => t a -> Int
length xs = foldr (\a b -> b + 1) 0 xs

main = do
  print $ length (1, 2)
  print $ length [(1, 2), (3, 4), (5, 6)]
  print $ fmap length [(1, 2), (3, 4), (5, 6)]
  print $ fmap length Just [1, 2, 3]
  print $ fmap length [Just 1, Just 2, Just 3]
  print $ fmap length [Just 1, Just 2, Nothing]
  