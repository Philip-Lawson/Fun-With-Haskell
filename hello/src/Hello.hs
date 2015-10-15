module Hello where

prompt :: IO ()
prompt = putStrLn "Please enter your name: "

sayHello :: String -> IO ()
sayHello name = putStrLn ("Hello " ++ name ++ "!")

checkBool :: IO Bool
checkBool = do c   <- getChar
               c'  <- getChar
               return (c == c')
