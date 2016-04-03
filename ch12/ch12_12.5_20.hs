data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)
  

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f b = case f b of
                Nothing -> Leaf
                Just (x, y, z) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f m
          | m == n = Nothing
          | otherwise = Just (m + 1, m, m + 1)
