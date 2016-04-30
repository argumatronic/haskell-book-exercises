import Data.Monoid ((<>))
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (traversable)

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable List where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse f Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Nil),
               (10, return (Cons x y))]

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

main = do
  let trigger = undefined :: List (Int, Int, [Int])
  quickBatch (traversable trigger)