import Data.Monoid ((<>))
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (traversable)

data Three' a b =
  Three' a b b
  deriving (Eq, Ord, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = (f b) <> (f c)

instance Traversable (Three' a) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse f (Three' a b c) = (Three' a) <$> f b <*> f c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

main = do
  let trigger = undefined :: Three' Int (Int, String, [Int])
  quickBatch (traversable trigger)