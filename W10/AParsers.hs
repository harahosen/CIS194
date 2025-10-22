{-# OPTIONS_GHC -Wall #-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- note: I am doing this homework after something like two-months break from Haskell, 
-- I am writing down different ways to do the same thing in order to refresh concepts and syntax

-- Exercise 1: Functor for Parser
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  -- using a lambda function for the first parsing:
  fmap f (Parser p) = Parser $ \input -> fmap (first f) (p input)
  -- same as above, without the lambda:
  -- fmap f (Parser p) = Parser (fmap (first f) . p)
  -- same as above, but using the <$> operator instead of fmap:
  -- fmap g (Parser p) = Parser $ \input -> first g <$> p input

  -- bschwb and surganov are more elegant, in my opinion:
  -- fmap f p = Parser $ fmap (first f) . runParser p
  -- fmap g (Parser f) = Parser $ fmap (first g) . f

-- Exercise 2: Applicative for Parser


-- https://github.com/OctaviPascual/cis194-IntroductionToHaskell/tree/master/homework-10
-- https://github.com/bschwb/cis194-solutions/tree/main/10-applicative-functors-part1
-- https://github.com/surganov/cis194/tree/master/10