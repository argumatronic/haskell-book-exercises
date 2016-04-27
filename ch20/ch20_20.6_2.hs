import Data.Monoid
import Data.Foldable

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

main = do
  print $ foldMap Sum (Two 1 2)
  print $ foldMap Any (Two "a" False)
