foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc bt = foldr f acc (flattenIn bt [])


flattenIn :: BinaryTree a -> [a] -> [a]
flattenIn Leaf l = l
flattenIn (Node left a right) l = flattenIn left (a : (flattenIn right l))