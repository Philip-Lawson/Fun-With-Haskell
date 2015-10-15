import Data.List
import Data.Ord
import Data.Char

separator = "-------------------------------------------"

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol lol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty Lol",
         "Lol ya",
         "Haha thanks just making sure rofl ur turn"]

type Digits = Int
type Presses = Int

data DaPhone = DaPhone [Button [Char]]
               deriving (Show)

data Button a =  One a
             | Two a
             | Three a
             | Four a
             | Five a
             | Six a
             | Seven a
             | Eight a
             | Nine a
             | Hash a
             | Zero a
             deriving (Show, Eq)

one = One "1"
two = Two "abc2"
three = Three "def3"
four = Four "ghi4"
five = Five "jkl5"
six = Six "mno6"
seven = Seven "pqrs7"
eight = Eight "tuv8"
nine = Nine "wxyz9"
hash = Hash ".,#"
zero = Zero " +_0"

phone :: DaPhone
phone = DaPhone [one, two, three, four, five, six, seven, eight, nine, hash, zero]

fingerTaps :: [(Digits, Presses)] -> Presses
fingerTaps list = sum $ map (snd) list

popularest :: Ord a => [a] -> a
popularest str = head $ maximumBy (comparing length) $ groupBy (==) (sort str)

coolestLtr :: [String] -> Char
coolestLtr = popularest . concat

coolestWord :: [String] -> String
coolestWord list = popularest (concatMap words list)
