import Data.Monoid
import Data.Foldable hiding (sum)
import Prelude hiding (sum)

sum :: (Foldable t, Num a) => t a -> a
sum x = getSum $ foldMap Sum x

main = do
  print $ sum [1,2,3] == 6
  print $ sum (Just 3) == 3
  print $ sum Nothing == 0
  print $ fmap sum (Just [1,2,3]) == Just 6