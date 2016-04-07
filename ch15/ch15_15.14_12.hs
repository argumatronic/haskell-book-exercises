
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>))

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
  Semigroup (AccumulateBoth a b) where
    (AccumulateBoth (Success a)) <> (AccumulateBoth (Success b))  = AccumulateBoth (Success (a <> b))
    (AccumulateBoth (Failure a)) <> (AccumulateBoth (Failure b))  = AccumulateBoth (Failure (a <> b))
    _                            <> (AccumulateBoth (Failure a))  = AccumulateBoth (Failure a)
    (AccumulateBoth (Failure a)) <> _                             = AccumulateBoth (Failure a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(AccumulateBoth (Success a)), (AccumulateBoth (Failure b))]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type AccumulateBothAssoc = AccumulateBoth String Ordering -> AccumulateBoth String Ordering -> AccumulateBoth String Ordering -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)