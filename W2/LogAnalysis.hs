{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Ex. 1

-- The last condition is due to the definition of the MessageType in Log.hs file
-- I think it could make sense to add the Unknonw type to the MessageType instead of having it in LogMessage
-- Anyhow... I know this is not a beautiful solution but I didn't want to change the original file and I still wanted to playing around defining lower level functions that could be called inside the parseMessage
getMessageCategory :: String -> MessageType
getMessageCategory str
 | head str == 'I' = Info
 | head str == 'W' = Warning
 | head str == 'E' = Error (read (words str !! 1))
 | otherwise = Error (-1)

getSeverity :: MessageType -> Int
getSeverity (Error severity) = severity
getSeverity _ = 0

getCurrentTime :: String -> MessageType -> Int
getCurrentTime str (Error _) = read (words str !! 2)
getCurrentTime str _ = read (words str !! 1)

getMessage :: String -> MessageType -> String
getMessage str (Error _) = unwords (drop 3 (words str))
getMessage str _ = unwords (drop 2 (words str))

parseMessage :: String -> LogMessage
parseMessage str 
 | severity == -1 = Unknown str
 | otherwise = LogMessage category timestamp message
 where
  category = getMessageCategory str
  severity = getSeverity category
  timestamp = getCurrentTime str category
  message = getMessage str category

parse :: String -> [LogMessage]
parse f = map parseMessage (lines f)

-- call for testing:
-- testparse parse 10 "error.log"



-- Ex 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) branch = branch
insert logMsg Leaf = Node Leaf logMsg Leaf
insert _ branch@(Node _ (Unknown _) _) = branch
insert logMsg1@(LogMessage _ timestamp1 _) (Node leftBranch logMsg2@(LogMessage _ timestamp2 _) rightBranch)
 | timestamp1 < timestamp2 = Node (insert logMsg1 leftBranch) logMsg2 rightBranch
 | otherwise = Node leftBranch logMsg2 (insert logMsg1 rightBranch)



-- Ex 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf



-- Ex 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftBranch root rightBranch) = inOrder leftBranch ++ [root] ++ inOrder rightBranch



-- Ex 5

getWrong :: LogMessage -> Bool
getWrong (LogMessage (Error severity) _ _) = severity >= 50
getWrong _ = False

getRight :: LogMessage -> Bool
getRight (LogMessage (Error severity) _ _) = severity < 50
getRight _ = False

getInfo :: LogMessage -> Bool
getInfo (LogMessage Info _ _) = True
getInfo _ = False

getWarning :: LogMessage -> Bool
getWarning (LogMessage Warning _ _) = True
getWarning _ = False

getNote :: LogMessage -> String
getNote (Unknown note) = note
getNote (LogMessage _ _ note) = note

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMsgs = [getNote logMsg | logMsg <- inOrder (build logMsgs), getWrong logMsg]

whatWentGood :: [LogMessage] -> [String]
whatWentGood logMsgs = [getNote logMsg | logMsg <- inOrder (build logMsgs), getRight logMsg]

collectInfo :: [LogMessage] -> [String]
collectInfo logMsgs = [getNote logMsg | logMsg <- inOrder (build logMsgs), getInfo logMsg]

collectWarning :: [LogMessage] -> [String]
collectWarning logMsgs = [getNote logMsg | logMsg <- inOrder (build logMsgs), getWarning logMsg]

-- call for testing:
-- testWhatWentWrong parse whatWentWrong "error.log"
-- testWhatWentWrong parse whatWentRight "error.log"
-- testWhatWentWrong parse collectInfo "error.log"
-- testWhatWentWrong parse collectWarning "error.log"

