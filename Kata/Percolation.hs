percolateKata :: [[Char]] -> Bool
percolateKata [[]]   = True
percolateKata (x:xs) = percolateKata' filledList
                        where filledList = (fillWithWater x): xs

percolateKata' :: [[Char]] -> Bool
percolateKata' [[]]     = True
percolateKata' [list]   = containsWater list
percolateKata' (x:y:xs) | (containsWater trickledList) = percolateKata' (trickledList: xs)                          
                        | otherwise = False
                           where trickledList = (spreadWaterLeft . spreadWaterRight) (trickleWaterDown x y)

containsWater :: [Char] -> Bool
containsWater []     = False
containsWater (x:xs) | x == 'W' = True
                     | otherwise = containsWater xs  

fillWithWater :: [Char] -> [Char]
fillWithWater []     = []
fillWithWater (x:xs) | x == '0'  = 'W': fillWithWater xs
                     | otherwise = x: fillWithWater xs  

trickleWaterDown :: [Char] -> [Char] -> [Char]
trickleWaterDown [] _          = []
trickleWaterDown _ []          = []
trickleWaterDown (x:xs) (y:ys) | x == 'W' && y == '0' = x: trickleWaterDown xs ys
                               | otherwise            = y: trickleWaterDown xs ys
                      
spreadWaterLeft :: [Char] -> [Char]
spreadWaterLeft []   = []
spreadWaterLeft [a]  = [a]
spreadWaterLeft list = (reverse . spreadWaterRight . reverse) list

spreadWaterRight :: [Char] -> [Char]
spreadWaterRight []       = []
spreadWaterRight [a]      = [a]
spreadWaterRight (x:y:xs) | x == 'W' && y == '0' = x: spreadWaterRight (x:xs)
                          | otherwise            = x: spreadWaterRight (y:xs)  









