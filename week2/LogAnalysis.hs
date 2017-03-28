{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where


import Log

parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
                   ("I":timestamp:msg) -> LogMessage Info (read timestamp) (unwords msg)
                   ("W":timestamp:msg) -> LogMessage Warning (read timestamp) (unwords msg)
                   ("E":err:timestamp:msg) -> LogMessage (Error (read err)) (read timestamp) (unwords msg)
                   _ -> Unknown s
                   

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ timestamp _) (Node left ilm@(LogMessage _ root _) right)
  | timestamp <= root = Node (insert lm left) ilm right
  | otherwise = Node left ilm (insert lm right)
insert (LogMessage _ _ _) (Node _ (Unknown _) _) = undefined

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logmsgs = case (inOrder (build logmsgs)) of
                          ((LogMessage (Error s) _ msg):rest) | s >= 50 -> msg:(whatWentWrong rest)
                          (_:rest) -> whatWentWrong rest
                          [] -> []
