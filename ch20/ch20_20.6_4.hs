import Data.Monoid
import Data.Foldable

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> f c

main = do
  print $ foldMap Sum (Three' "a" 1 2)
  print $ foldMap Any (Three' 1 True False)
  