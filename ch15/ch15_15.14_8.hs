
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine {unCombine=f} <> Combine {unCombine=g} = Combine (f <> g)

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--   arbitrary = do
--     a <- arbitrary
--     b <- arbitrary
--     elements [(Fst a), (Snd b)]

-- instance CoArbitrary a => CoArbitrary (Combine a b) where
--   coarbitrary Combine { unCombine = (a -> b) }  = variant 0
--   coarbitrary (Just x) = variant 1 . coarbitrary x

-- semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
-- semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- type CombineAssoc = Combine String Ordering -> Combine String Ordering -> Combine String Ordering -> Bool

-- main :: IO ()
-- main =
--   quickCheck (semigroupAssoc :: CombineAssoc)

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

main = do
  print $ unCombine (f <> g ) $ 0
  print $ unCombine (f <> g ) $ 1
  print $ unCombine (f <> f ) $ 1
  print $ unCombine (g <> f ) $ 1
