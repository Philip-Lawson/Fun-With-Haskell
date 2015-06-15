-- Problem 1
-- (*) Find the last element of a list.

findLast :: [a] -> a
findLast [] = error "This list is empty!"
findLast x = last x

findLastRecursively :: [a] -> a
findLastRecursively [] = error "This list is empty!"
findLastRecursively x = if length x == 1
                        then x !! 0
                      else findLastRecursively (tail x) 

-- Problem 2
-- (*) Find the last but one element of a list.

findSecondLast :: [a] -> a
findSecondLast [] = error "This is an empty list!"
findSecondLast x = findLastRecursively(init x)

-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.

elementAt :: Int -> [a] -> a
elementAt x [] = error "This list is empty!"
elementAt x xs = if x > length xs || x < 1
                    then error "The index is out of range!"
                 else xs !! (x-1)

-- Problem 4
-- (*) Find the number of elements of a list.

findLength :: [a] -> Int
findLength [] = 0
findLength list = 1 + findLength (init list)
findLength' list = sum [1 | _ <- list] 

-- Problem 5
-- (*) Reverse a list.

listReverser :: [a] -> [a]
listReverser [] = error "This list is empty"
listReverser [a] = [a]
listReverser list = [last list] ++ listReverser(init list)

-- Problem 6
-- (*) Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. (x a m a x).

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome list = (head list) == (last list) && isPalindrome ( tail (init list) )
isPalindrome' list = list == (reverse list)


-- Problem 7
-- (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

-- define Nested List for question
data NestedList a = Elem a | List[NestedList a]

flattenList :: NestedList a -> [a]
flattenList (Elem a) = [a]
flattenList (List (x:xs)) = flattenList x ++ flattenList (List xs)
flattenList (List []) = []
              
-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
      
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:xs) = if x == head xs
                     then compress xs
                  else x: compress xs
                      
