import Data.Monoid
import Data.Foldable

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = f b <> f c <> f c

main = do
  print $ foldMap Sum (Four' "a" 1 2 10)
  print $ foldMap Any (Four' 1 True False True)
  