import Data.Monoid
import Data.Foldable hiding (maximum)
import Prelude hiding (maximum)


newtype Max a = Max {getMax :: Maybe a}

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing

  m `mappend` Max Nothing = m
  Max Nothing `mappend` n = n
  (Max m@(Just x)) `mappend` (Max n@(Just y))
    | x >= y    = Max m
    | otherwise = Max n

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs = getMax $ foldMap (\a -> Max {getMax = Just a}) xs

main = do
  print $ maximum "julie"
  print $ fmap maximum (Just "julie")
  print $ fmap maximum [Just 'j', Just 'u', Just 'l']
  print $ fmap maximum [Just 4, Just 3, Nothing]