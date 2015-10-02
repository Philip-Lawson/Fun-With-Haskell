import Data.Char

sumList :: Num a => [a] -> a -- type declaration
sumList [] = 0
sumList [a] = a
sumList (x:xs) = x + sumList xs 

concatDuplicates :: Eq a => [a] -> ([a],[a])
concatDuplicates[] = ([], [])
concatDuplicates[a] = ([a], [])
concatDuplicates list = span (== head list) list

numberOfDuplicates :: ([a], [a]) -> (Int, a)
numberOfDuplicates ([],[]) = error "Bad input"
numberOfDuplicates tuple = ( length (fst tuple), head (fst tuple))

duplicateSets :: Eq a => [a] -> [[a]]
duplicateSets [a] = [[a]]
duplicateSets list = [fst (concatDuplicates list)] ++ duplicateSets (snd (concatDuplicates list))
       
-- indexSets :: Eq a => [[a]] -> [(Int, a)]
-- indexSets [[a]] = [(length a, head a)]
-- indexSets (x:xs) = [(length x, head x)] ++ [indexSets xs ]   

replace :: Eq a => a -> a-> [a] -> [a]
replace a b [] = []
replace a b [x] = if x == a
                  then [b]
                  else [x]
replace a b (x:xs) 
           | x == a = [b] ++ replace a b xs
           | otherwise = [x] ++ replace a b xs         

mergeList        :: Ord a => [a] -> [a] -> [a]
mergeList [] []   = []
mergeList [] list = list
mergeList list []  = list 
mergeList (x:xs) (y:ys) | x < y     = x: mergeList xs (y:ys)
                        | otherwise = y: mergeList (x:xs) ys

mergeList'          :: Ord a => [[a]] -> [a]
mergeList' [[]]     = []
mergeList' [(x:xs)] = (x:xs)
mergeList' list   = mergeList' (mergedLists list)

mergedLists        :: Ord a => [[a]] -> [[a]]
mergedLists [[]]   = [[]]
mergedLists [list]  = [list]
mergedLists [listA,listB] = [mergeList listA listB]
mergedLists (x:y:xs) = mergeList x y: mergedLists xs

decompose       :: Ord a => [a] -> [[a]]
decompose []     = [[]]
decompose [a]    = [[a]]
decompose (x:xs) = [x]: decompose xs
          
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort list = mergeList' (decompose list)            
                      

toDigits :: Integer -> [Integer]
toDigits x | x <= 0 = []
           | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsReverse :: Integer -> [Integer]
toDigitsReverse x | x <= 0 = []
                 | otherwise = x `mod` 10: toDigitsReverse (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [a] = [a]
doubleEveryOther (x:xs) | (length xs + 1) `mod` 2 == 0 = x*2 : doubleEveryOther xs
                        | otherwise = x: doubleEveryOther xs


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits list = foldl1 (+) list

validate :: Integer -> Bool
validate x = sumDigits(doubleEveryOther( toDigits x )) `mod` 10 == 0

isPalindrome :: String -> Bool

isPalindrome string = preparedString == (reverse preparedString)
                     
                where preparedString = (map (toUpper) . filter (/= ' ')) string





 

