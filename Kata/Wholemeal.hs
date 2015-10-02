data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
           deriving (Show, Eq)


fun1 :: [Integer] -> Integer
fun1 = foldr (*) 1 . map (subtract 2) . filter (even)

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/=1) . iterate (\n -> if even n then n `div` 2 else n * 3 + 1)

insert :: Ord a => Tree a -> Integer -> a -> Tree a
insert (Leaf) num a = Node num (Leaf) a (Leaf)
insert (Node depth left thing right) num a | a > thing = Node depth left thing (insert right (depth+1) a)
                                           | a < thing = Node depth (insert left (depth+1) a) thing right
										   | otherwise = Node depth left thing right


xor :: [Bool] -> Bool
xor list = odd $ foldr (+) 0 . map (\x -> if x then 1 else 0) $ list  
