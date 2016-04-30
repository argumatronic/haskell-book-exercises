{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid ((<>))
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (traversable)

data S n a = S (n a) a
  deriving (Eq, Ord, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = (foldMap f na) <> (f a)

instance Traversable n => Traversable (S n) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse f (S na a) = S <$> traverse f na <*> f a

instance Arbitrary a => Arbitrary (S [] a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (S [a] b)

instance Eq a => EqProp (S [] a) where (=-=) = eq

main = do
  let trigger = undefined :: S [] (Int, String, [Int])
  quickBatch (traversable trigger)