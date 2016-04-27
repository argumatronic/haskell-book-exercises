import Data.Monoid
import Data.Foldable

data Constant a b =
  Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

main = do
  print $ foldMap Sum (Constant 1)
  -- Sum {getSum = 0}
  print $ foldMap Any (Constant 1)
  -- Any {getAny = False}
  print $ foldMap (\x -> Any True) (Constant 1)
  -- Any {getAny = False}