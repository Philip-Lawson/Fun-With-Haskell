module Test where

testpi :: Fractional a => Double 
testpi = 3.14

sayHello :: String -> IO ()
sayHello x = putStrLn("Hello, " ++ x ++ "!")

triple :: Fractional a => a -> a 
triple = (*3)

half :: Fractional a => a -> a 
half = (/2)

square :: Fractional a => a -> a 
square = (^2)

bypi :: Fractional a => a -> a 
bypi = (*) 3.14 . square
