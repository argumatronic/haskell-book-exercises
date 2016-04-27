import Data.Monoid
import Data.Foldable

filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF f xs = foldMap fb xs
  where fb x = if f x -- (a -> Bool), here I mixed the types before
                then mempty -- Monoid
                else pure x -- Applicative

main = do
  print $ filterF odd [1..10]
