
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))
import Data.Monoid (Monoid)

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine {unCombine=f} <> Combine {unCombine=g} = Combine (f <> g)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

main = do
  print $ unCombine (f <> g ) $ 0
  print $ unCombine (f <> g ) $ 1
  print $ unCombine (f <> f ) $ 1
  print $ unCombine (g <> f ) $ 1
  print $ unCombine (mappend mempty f) 0 == unCombine f 0
