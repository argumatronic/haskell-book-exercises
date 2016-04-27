import Prelude hiding (Left, Right) 

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data PhhhbbtttEither b a =
    Left a
  | Right b 
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left b, Right a]

instance Functor (PhhhbbtttEither a) where
  fmap _ (Right a) = Right a
  fmap f (Left a) = Left (f a)

instance Applicative (PhhhbbtttEither a) where
  pure = Left
  (<*>) _ (Right a) = Right a
  (<*>) (Right a) _ = Right a
  (<*>) (Left f) (Left b) = Left (f b)

instance Monad (PhhhbbtttEither a) where
  return = pure
  (>>=) (Right a) _ = Right a
  (>>=) (Left a) f = f a

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where (=-=) = eq

main = do
  let trigger = undefined :: PhhhbbtttEither String (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger