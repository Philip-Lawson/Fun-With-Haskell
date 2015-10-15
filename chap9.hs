import Data.Char

newtype Word' =
   Word' String
   deriving (Eq, Show)

data Nat =
     Zero
   | Succ Nat
   deriving (Eq, Show)

vowels :: String
vowels = "aeiou" 

mkWord :: String -> Maybe Word'
mkWord str | countCons str > countVowels str = Just (Word' str)
           | otherwise = Nothing

countCons :: String -> Int
countCons str = length $ filter (\x -> isAlpha x && not (elem x vowels)) $ map toLower str

countVowels :: String -> Int
countVowels str = length $ filter (\x -> elem x vowels) $ map toLower str

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str = Just str

replaceThe :: String -> String
replaceThe str = unwords $ map (\x -> if x == "the" then "a" else x) (words  str)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go (words (map toLower str)) 0
                        where go :: [String] -> Integer -> Integer
                              go (x:y:ys) n | x == "the" && elem (head y) vowels = go (y:ys) (n+1) 
                                            | otherwise = go (y:ys) n
                              go _ n = n

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n | n < 0 = Nothing
               | otherwise = Just (go n)
                  where go 0 = Zero
                        go n = Succ (go (n-1))
