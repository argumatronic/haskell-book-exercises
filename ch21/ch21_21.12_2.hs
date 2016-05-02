import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (traversable)

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

type TI a = Constant a

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a
  -- does not work, why?
  -- fmap _ c = c

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse _ (Constant a) = pure $ Constant a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return (Constant a)

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq

main = do
  let trigger = undefined :: TI Int (Int, Int, [Int])
  quickBatch (traversable trigger)