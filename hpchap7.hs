dodgy :: Num a => a -> a -> a
dodgy x y = x + ( *) 10 y

oneIsOne :: Num a => a -> a 
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a 
oneIsTwo = (flip dodgy) 2
