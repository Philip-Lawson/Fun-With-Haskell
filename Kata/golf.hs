import Data.List
import Data.Ord

type Frequency = Int
histoBottom = "==========\n0123456789\n"

skips:: [a] -> [[a]]
skips list = map (bites list) [0..(length list - 1)]
	where bites list a | length list == 0 = []
					   | otherwise        = (take 1 . drop a) list ++ bites (drop (a+1) list) a 

localMaxima:: [Integer] -> [Integer]
localMaxima list | length list < 3        = []
				 | isMaxima (take 3 list) = list!!1: localMaxima (tail list)
				 | otherwise              = localMaxima (tail list)
				  where isMaxima [a,b,c] = b > a && b > c
				  
histogram:: [Int] -> String
histogram list = intercalate "\n" $ reverse $ histoBottom:histoList
				where histoList = map (toHistogramLevel freqList) [1..(maximum (map (fst) freqList))]
				      freqList = getFrequencies list

toHistogramLevel:: [(Frequency, Int)] -> Frequency -> String
toHistogramLevel list num = concatMap (\x -> if fst x >= num then "*" else " ") paddedList
					 where paddedList = filter (\x -> snd x >= 0 && snd x < 10) $ padList list
					 
padList:: [(Frequency, Int)] -> [(Frequency, Int)]
padList list = sortBy (comparing snd) (padList' 0 10 list)

padList':: Int -> Int -> [(Frequency, Int)] -> [(Frequency, Int)]
padList' x y list | x == y                  = list
				  | elem x $ map (snd) list = padList' (x+1) y list
                  | otherwise               = (0,x):padList' (x+1) y list
               

getFrequencies:: Ord a => [a] -> [(Frequency, a)]
getFrequencies = map (\x -> (length x, head x)) . group . sort 
               