{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
parse:: String -> [LogMessage]
parse string =  (parseMessages . lines) string

parseMessages:: [String] -> [LogMessage]
parseMessages [] = []
parseMessages [a] = [parseMessage a]
parseMessages (x:xs) = parseMessage x: parseMessages xs 


parseMessage:: String -> LogMessage
parseMessage string | (isLogMessage . head . words) string = getLogMessage (words string)
                    | otherwise = Unknown string

getLogMessage:: [String] -> LogMessage
getLogMessage [] = Unknown " "
getLogMessage (x:y:xs) | x == "E" = LogMessage (getMessageType [x,y]) (getTimeStamp(head xs)) (unwords (tail xs) )
                       | otherwise = LogMessage (getMessageType [x]) (getTimeStamp y) (unwords xs)
getLogMessage list = Unknown (unwords list)
 

getMessageType:: [String] -> MessageType
getMessageType [] = error "no message"
getMessageType [a]    | a == "I" = Info
                      | a == "W" = Warning
                      | otherwise = error "bad message"
getMessageType (x:xs) | x == "E" = Error ( (read (head xs) ::Int))
                      | otherwise = getMessageType [x]

isLogMessage:: String -> Bool
isLogMessage string = string == "I" || string == "E" || string == "W" 

getTimeStamp:: String -> Int
getTimeStamp string = (read string)::Int

parseTimeStamp:: LogMessage -> TimeStamp
parseTimeStamp (Unknown _)                      = error "no timestamp"
parseTimeStamp (LogMessage _ timeStamp _)       = timeStamp

insert:: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message (Leaf)   = (Node Leaf message Leaf)
insert message (Node leftTree treeMessage rightTree) 
     | parseTimeStamp message > parseTimeStamp treeMessage = Node leftTree treeMessage (insert message rightTree) 
     | parseTimeStamp message < parseTimeStamp treeMessage = Node (insert message leftTree) treeMessage rightTree
	 | otherwise 										   = Node leftTree treeMessage rightTree

build:: [LogMessage] -> MessageTree
build [] = error "no log messages"
build [a]    = insert a Leaf
build (x:xs) = insert x (build xs)

inOrder:: MessageTree -> [LogMessage]
inOrder Leaf                      = []
inOrder (Node Leaf message Leaf)  = [message]
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

whatWentWrong:: [LogMessage] -> [String]
whatWentWrong []   = [""]
whatWentWrong list = map (getMessage) (filter (isSevereError) messageList)
             where messageList = inOrder (build list)

isSevereError:: LogMessage -> Bool
isSevereError (LogMessage (Error num) _ _) = num >= 50
isSevereError _                            = False

getMessage:: LogMessage -> String
getMessage (Unknown _) = ""
getMessage (LogMessage _ _ string) = string