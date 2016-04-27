import Data.Monoid
import Data.Foldable hiding (null)
import Prelude hiding (null)

null :: (Foldable t) => t a -> Bool
null x = length x == 0

main = do
  print $ null (Left 3)
  print $ null []
  print $ null Nothing
  print $ null (1, 2)
  print $ fmap null [Just 1, Just 2, Nothing]