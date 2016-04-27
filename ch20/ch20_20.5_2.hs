import Data.Monoid
import Data.Foldable hiding (product)
import Prelude hiding (product)

product :: (Foldable t, Num a) => t a -> a
product x = getProduct $ foldMap Product x

main = do
  print $ product [10,2,3]
  print $ product (Just 3)
  print $ product Nothing
  print $ fmap product (Just [10,2,3])