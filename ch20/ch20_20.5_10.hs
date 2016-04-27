import Data.Monoid
import Data.Foldable hiding (foldMap)
import Prelude hiding (foldMap)

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f xs = foldr (\x y -> f x <> y) mempty xs

main = do
  print $ foldMap Sum [1, 2, 3]
