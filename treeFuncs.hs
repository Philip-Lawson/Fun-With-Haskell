data BinaryTree a = 
     Leaf
   | Node (BinaryTree a) a (BinaryTree a)
   deriving (Eq, Show, Ord)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
   | b == a = Node left a right
   | b < a  = Node (insert' b left) a right
   | b > a  = Node left a (insert' b right)

mapTree :: (Ord a, Ord b) => (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

filterTree :: Ord a => (a -> Bool) -> BinaryTree a -> BinaryTree a
filterTree pred Leaf = Leaf
filterTree pred (Node left a right) 
   | pred a = Node (filterTree pred left) a (filterTree pred right)
   | otherwise = Leaf

testFilter :: BinaryTree Integer
testFilter = 
   Node
   (Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf))
    0
    (Node (Node Leaf 5 Leaf) 2 (Node Leaf 6 Leaf))

treeToList :: BinaryTree a -> [a]
treeToList Leaf = []
treeToList (Node left a right) = treeToList left ++ [a] ++ treeToList right

filterExpected =
   Node (Node Leaf 1 Leaf) 0 (Node Leaf 2 Leaf)

filterOkay =
   if (filterTree (<3) testFilter == filterExpected)
   then print "yup filter okay!"
   else error "filter test failed!"

testMap :: BinaryTree Integer
testMap = 
   Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
   Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = 
   if mapTree (+1) testMap == mapExpected
   then print "yup map okay!"
   else error "map test failed!"

listExpected = [1,3,4]

testToList = insert' 1 (insert' 3 (insert' 4 Leaf))

toListOkay = 
   if treeToList testToList == listExpected
   then print "yup tolist okay!"
   else error "tolist test failed!"

main = do 
         filterOkay
         mapOkay
         toListOkay
