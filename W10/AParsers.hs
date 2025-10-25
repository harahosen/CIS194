{-# OPTIONS_GHC -Wall #-}

module AParser where

import           Control.Applicative
import           Control.Monad

import           Data.Char
import Data.Binary.Get (remaining)
import qualified Wholemeal as 3

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

-- Ex. 1: Functor for Parser
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  -- using a lambda function for the first parsing:
  fmap f (Parser p) = Parser $ \input -> fmap (first f) (p input)
  -- same as above, without the lambda:
  -- fmap f (Parser p) = Parser (fmap (first f) . p)
  -- same as above, but using the <$> operator instead of fmap:
  -- fmap f (Parser p) = Parser $ \input -> first f <$> p input

  -- bschwb and surganov are more elegant, in my opinion:
  -- fmap f p = Parser $ fmap (first f) . runParser p
  -- fmap g (Parser f) = Parser $ fmap (first g) . f

-- Ex. 2: Applicative for Parser
-- pure  :: a -> Parser a
-- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b [bind operator from the Monad instance of Maybe]
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c [Kleisli composition operator from the Control.Monad module]

-- working on this, I found out about the >=> operator (https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Monad.html#v:-62--61--62-)
-- so in the end this is my final version
instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  Parser parserFunction <*> Parser parserValue = Parser $ parserFunction >=> apply
    where
      apply (parsedFunction, remainingInput) = first parsedFunction <$> parserValue remainingInput


-- Ex. 3: multiple parsers

-- 3.a
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- 3.b
abParser_ :: Parser ()
-- abParser_ = const () <$> abParser
abParser_ = char 'a' *> char 'b' *> pure ()

-- 3.c
intPair :: Parser [Integer]
intPair = (:) <$> posInt <*> (char ' ' *> posInt)
-- intPair = (\a _ b -> [a,b]) <$> posInt <*> char ' ' <*> posInt


-- Ex. 4: Alternative instance of Parser

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser parserFunction1 <|> Parser parserFunction2 = Parser $ \input -> parserFunction1 input <|> parserFunction2 input

-- Ex. 5: integer values or uppercase character parser
intOrUppercase :: Parser ()
--intOrUppercase = void posInt <|> void (satisfy isUpper)
intOrUppercase = (() <$ posInt) <|> (() <$ satisfy isUpper)

