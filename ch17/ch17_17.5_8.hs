import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- append :: List a -> List a -> List a
-- append Nil ys = ys
-- append (Cons x xs) ys = Cons x $ xs `append` ys

instance Monoid (List a) where
  mempty = Nil
  mappend a Nil = a
  mappend Nil a = a
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Arbitrary a => Arbitrary (List a) where
  -- arbitrary = Cons <$> arbitrary <*> arbitrary
  -- arbitrary = do
  --   x <- arbitrary
  --   return (Cons x Nil)
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Cons x (Cons y Nil))

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f b) ca = fmap f ca <> (b <*> ca)

instance Eq a => EqProp (List a) where (=-=) = eq

-- quickBatch $ applicative [("b", "w", 1)]
-- quickBatch $ applicative (Just ("b", "w", 1))
-- quickBatch $ applicative (Cons ("b", "w", 1) Nil)

functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)
res = functions <*> values
