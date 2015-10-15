type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy x y
         | x < y = 0
         | otherwise = 1 + dividedBy (x - y) y

foldBool :: a -> a -> Bool -> a
foldBool x y bool
               | bool = x
               | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read a) => a -> a 
roundTrip = read . show



main = do
   print (roundTrip (4 :: Int))
   print (id 4)
