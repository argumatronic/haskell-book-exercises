
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>))

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a =>
  Semigroup (Validation a b) where
    (Failure a) <> (Failure b)  = Failure (a <> b)
    (Failure a) <> _            = Failure a
    _           <> (Failure a)  = Failure a
    a           <> _            = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Success a), (Failure b)]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type ValidationAssoc = Validation String Ordering -> Validation String Ordering -> Validation String Ordering -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: ValidationAssoc)