import Data.Monoid
import Data.Foldable hiding (fold)
import Prelude hiding (fold)

-- foldMap :: Monoid m => (a -> m) -> t a -> m

fold :: (Foldable t, Monoid m) => t m -> m
fold xs = foldMap id xs

main = do
  print $ fold ["a", "b", "c"]
