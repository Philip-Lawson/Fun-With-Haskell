module Main where

import Hello
import System.IO

main :: IO ()
main = do
   hSetBuffering stdout NoBuffering
   prompt
   name <- getLine
   sayHello name 
