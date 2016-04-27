import Data.Monoid
import Data.Foldable hiding (elem)
import Prelude hiding (elem)

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem el xs = getAny $ foldMap (\e -> Any $ e == el ) xs

main = do
  print $ elem 2 (Just 3)
  print $ elem True (Right True)
  print $ fmap (elem 3) [Right 1, Right 2, Right 3]
