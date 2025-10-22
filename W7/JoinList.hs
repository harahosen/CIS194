{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Ex. 1: JoinList append function

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b


-- Ex. 2: JoinList size function

-- function defined on the homework file
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

sz :: (Monoid b, Sized b) => JoinList b a -> Int
sz = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
  | i == 0    = Just a
  | otherwise = Nothing
indexJ i (Append _ left right)
  | i < leftsize = indexJ i left
  | otherwise = indexJ (i - leftsize) right
  where
    leftsize = sz left

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i s@(Single _ _)
    | i <= 0 = s
    | otherwise = Empty
dropJ i (Append _ left right)
    | i <= 0 = Append (tag left <> tag right) left right
    | i >= leftsize = dropJ (i - leftsize) right
    | otherwise = dropJ i left +++ right
    where
      leftsize = sz left

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i s@(Single _ _)
    | i > 0 = s
    | otherwise = Empty
takeJ i (Append _ left right)
    | i <= 0 = Empty
    | i >= leftsize + rightsize = Append (tag left <> tag right) left right
    | i < leftsize = takeJ i left
    | otherwise = left +++ takeJ (i - leftsize) right
    where
      leftsize = sz left
      rightsize = sz right

-- Ex. 3: Scrabble
-- see Scrabble.hs

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Ex. 4: 

instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jlToList
    fromString xs = foldr ((+++) . createLine) Empty $ lines xs
        where createLine s = Single (scoreString s, Size 1) s
    replaceLine n str jl
        | n < 0 || n >= numLines jl = jl
        | otherwise = takeJ n jl +++ fromString str +++ dropJ (n + 1) jl
    line = indexJ
    numLines = sz
    value = getScore . fst . tag


main = runEditor editor jlbuffer 
  where jlbuffer = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: (JoinList (Score, Size) String)