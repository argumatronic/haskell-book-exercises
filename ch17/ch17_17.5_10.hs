import Control.Applicative
import Data.Monoid (Monoid, (<>))
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data Validation e a =
    Error e
  | Success a
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second a) = Second (f a)

instance Applicative (Sum a) where
  pure = Second
  (<*>) _ (First a) = First a
  (<*>) (First a) _ = First a
  (<*>) (Second f) (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

-- same as Sum/Either
instance Functor (Validation e) where
  fmap _ (Error e) = Error e
  fmap f (Success a) = Success (f a)

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Error e) (Error e') = Error (e <> e')
  (<*>) _ (Error e) = Error e
  (<*>) (Error e) _ = Error e
  (<*>) (Success f) (Success b) = Success (f b)

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

-- quickBatch $ applicative (Second ("b", "w", 1))
-- quickBatch $ applicative (Success ("b", "w", 1))