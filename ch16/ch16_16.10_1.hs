import Test.QuickCheck
import Test.QuickCheck.Function

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

-- functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
-- functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)
-- Prelude> let li x = functorCompose (+1) (*2) (x :: [Int])
-- Prelude> quickCheck li
-- +++ OK, passed 100 tests.
functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorCompose :: IntFC)
