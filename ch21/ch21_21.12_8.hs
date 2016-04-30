import Data.Monoid ((<>))
import Test.QuickCheck (Arbitrary, arbitrary,frequency)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (traversable)

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node tl a tr) = Node (fmap f tl) (f a) (fmap f tr)

-- foldMap is a bit easier and looks more natural,
-- but you can do foldr too for extra credit.
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node tl a tr) = (foldMap f tl) <> (f a) <> (foldMap f tr)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node tl a tr) = Node <$> (traverse f tl) <*> (f a) <*> (traverse f tr)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    x <- arbitrary
    l <- arbitrary
    r <- arbitrary
    frequency [(1, return Empty),
               (1, return (Leaf x)),
               (1, return (Node l x r))]

instance Eq a => EqProp (Tree a) where (=-=) = eq

main = do
  let trigger = undefined :: Tree (Int, String, [Int])
  quickBatch (traversable trigger)