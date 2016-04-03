foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a (foldTree f acc left) (foldTree f acc right)
