import Data.Monoid
import Data.Foldable hiding (minimum)
import Prelude hiding (minimum)

newtype Min a = Min {getMin :: Maybe a}

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  m `mappend` Min Nothing = m
  Min Nothing `mappend` n = n
  (Min m@(Just x)) `mappend` (Min n@(Just y))
    | x <= y    = Min m
    | otherwise = Min n

-- original minumum sigranute, t is Foldable, it comes from class
-- minimum :: forall a. Ord a => t a -> a
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs = getMin $ foldMap (\a -> Min {getMin = Just a}) xs

main = do
  print $ minimum "julie"
  print $ fmap minimum (Just "julie")
  print $ fmap minimum [Just 'j', Just 'u', Just 'l']
  print $ fmap minimum [Just 4, Just 3, Nothing]
  -- print $ minimum (Left 3)
  -- works only in GHCI, don't know why
  -- ends up with 
  -- No instance for (Ord a0) arising from a use of ‘minimum’
  -- but Either has Ord

