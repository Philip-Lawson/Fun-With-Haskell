recurseSum :: (Eq a, Num a, Enum a) => a -> a 
recurseSum n = sum [1..n]


-- This could be written a lot clearer using guards and allowing for an Ord constraint
-- I wanted to see if I could do it without using any comparisons (e.g. x < 0, x > 0) 
-- and still allow all types of multiplication
multSummation :: (Eq a, Num a) => a -> a -> a
multSummation 0 n = 0
multSummation n 0 = 0
multSummation x y = (x * signum y) + multSummation x (y - signum y)

digitToWord :: Int -> String
digitToWord x 
         | x == 1 = "-one"
         | x == 2 = "-two"
         | x == 3 = "-three"
         | x == 4 = "-four"
         | x == 5 = "-five"
         | x == 6 = "-six"
         | x == 7 = "-seven"
         | x == 8 = "-eight"
         | x == 9 = "-nine"
         | otherwise = "-zero"

digits :: Int -> [Int]
digits x = go (abs x)
      where go x
             | x == 0 = []
             | otherwise = go (x `div` 10) ++ [x `rem` 10]

wordNumber :: Int -> String
wordNumber x = tail $ concatMap digitToWord (digits x)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail [a] = Nothing
maybeTail (_:xs) = Just xs

myEnumFromTo :: (Enum a, Eq a, Ord a) => a -> a -> [a]
myEnumFromTo x y
             | x == y = [x]
             | x > y = x: myEnumFromTo (pred x) y
             | otherwise = x: myEnumFromTo (succ x) y

myLines :: String -> [String]
myLines "" = []
myLines string 
         | elem '\n' string  = takeWhile (/= '\n') string : myLines ( tail $ dropWhile (/= '\n') string)
         | otherwise = [string]

myFilter :: String -> [String]
myFilter string = filter (\x -> x /= "the" && x /= "a" && x /= "an") $ words string

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) 
         | x = True
         | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f list = myOr $ map f list

myElem :: Eq a => a  -> [a] -> Bool
myElem _ [] = False
myElem f (x:xs) 
            | f == x = True
            | otherwise = myElem f xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f list = squish $ map f list

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximum :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximum _ [] = Nothing
myMaximum f list = go f (tail list) (head list)
               where go f list acc  
                        | length list == 0 = Just acc
                        | (f (head list) acc) == GT = go f (tail list) (head list)
                        | otherwise = go f (tail list) acc

sentences = "Blah\nBlah\nBlah"
shouldEqual = ["Blah","Blah","Blah"]

main :: IO ()
main =
   print $ "Are they equal? "
            ++ show (myLines sentences == shouldEqual)
