import Test.QuickCheck
import Test.QuickCheck.Function

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorCompose :: IntFC)
