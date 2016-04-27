import Control.Applicative
import Data.Monoid (Monoid, (<>))
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a f f') (Three' a' b b') = Three' (a <> a') (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- error - the first value needs to me Monoid, but the 3rd value in the tuple is Num, no clear Monoid there
-- quickBatch $ applicative (Three' ("b", "w", 1) ("b", "w", 1) ("b", "w", 1))

-- quickBatch $ applicative (Three' "b" ("f", "f", 4) ("c", "h", 2))
-- quickBatch $ applicative (undefined :: Three' String (Int, Double, Char))