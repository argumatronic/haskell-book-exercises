import Data.Monoid
import Data.Foldable

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

main = do
  print $ foldMap Sum (Three "a" 1 2)
  print $ foldMap Any (Three 1 "a" False)
  