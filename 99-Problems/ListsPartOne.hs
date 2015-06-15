findLast :: [a] -> a
findLast [] = error "This list is empty!"
findLast x = last x

findLastRecursively :: [a] -> a
findLastRecursively [] = error "This list is empty!"
findLastRecursively x = if length x == 1
                        then x !! 0
                      else findLastRecursively (tail x) 

findSecondLast :: [a] -> a
findSecondLast [] = error "This is an empty list!"
findSecondLast x = findLastRecursively(init x)

elementAt :: Int -> [a] -> a
elementAt x [] = error "This list is empty!"
elementAt x xs = if x > length xs || x < 1
                    then error "The index is out of range!"
                 else xs !! (x-1)

findLength :: [a] -> Int
findLength [] = 0
findLength list = 1 + findLength (init list)
findLength' list = sum [1 | _ <- list] 

listReverser :: [a] -> [a]
listReverser [] = error "This list is empty"
listReverser [a] = [a]
listReverser list = [last list] ++ listReverser(init list)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome list = (head list) == (last list) && isPalindrome ( tail (init list) )
isPalindrome' list = list == (reverse list)

-- define Nested List for question
data NestedList a = Elem a | List[NestedList a]

flattenList :: NestedList a -> [a]
flattenList (Elem a) = [a]
flattenList (List (x:xs)) = flattenList x ++ flattenList (List xs)
flattenList (List []) = []
                    
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:xs) = if x == head xs
                     then compress xs
                  else x: compress xs
                      
