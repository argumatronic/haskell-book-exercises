
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>), Sum(..))
import Data.Monoid (Monoid)

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  Comp {unComp=f} <> Comp {unComp=g} = Comp (f <> g)

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)


f = Comp $ \(Sum n) -> Sum (n + 1)
g = Comp $ \(Sum n) -> Sum (n - 1)

main = do
  print $ unComp (f <> g ) $ 0
  print $ unComp (f <> g ) $ 1
  print $ unComp (f <> f ) $ 1
  print $ unComp (g <> f ) $ 1
  print $ unComp (mappend mempty f) 0 == unComp f 0