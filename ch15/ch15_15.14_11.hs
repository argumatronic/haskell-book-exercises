
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>))

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
    (AccumulateRight (Success a)) <> (AccumulateRight (Success b))  = AccumulateRight (Success (a <> b))
    (AccumulateRight (Failure a)) <> _                              = AccumulateRight (Failure a)
    _                             <> (AccumulateRight (Failure a))  = AccumulateRight (Failure a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(AccumulateRight (Success a)), (AccumulateRight (Failure b))]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type AccumulateRightAssoc = AccumulateRight String Ordering -> AccumulateRight String Ordering -> AccumulateRight String Ordering -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)