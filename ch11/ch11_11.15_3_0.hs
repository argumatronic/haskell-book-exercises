data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc bt = foldr f acc (inorder bt)

inorder :: BinaryTree a -> [a]
inorder bt = flattenIn bt []

flattenIn :: BinaryTree a -> [a] -> [a]
flattenIn Leaf l = l
flattenIn (Node left a right) l = flattenIn left (a : (flattenIn right l))

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt = foldTree mk Leaf bt
  where mk a t = Node l (f a) r

-- do test also
testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree == mapExpected
  then print "yup okay!"
  else error "test failed!"
