{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import ExprT
import Parser (parseExp)
import StackVM
import qualified Data.Map as M
import Control.Applicative(liftA2)


-- Ex. 1: eval
eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- Ex. 2: evalStr
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

-- Ex. 3: Expr
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul


-- Ex. 4: type instances
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit :: Integer -> Mod7
    lit n = Mod7 (mod n 7)
    add (Mod7 x) (Mod7 y) = Mod7 $ mod (x + y) 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x * y) 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

--testInteger = testExp :: Maybe Integer
--testBool = testExp :: Maybe Bool
--testMM = testExp :: Maybe MinMax
--testSat = testExp :: Maybe Mod7

-- Ex. 5: arithmetic expressions compiler
instance Expr Program where
    lit n = [StackVM.PushI n]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

{- if we want to implent also the boolean type and the and/or operations...
(not tested, the parseExp function needs to be modified accordingly)
-- modified Expr class:
class Expr a where
    lit  :: Integer -> a
    add  :: a -> a -> a
    mul  :: a -> a -> a
    bool :: Bool -> a
    andE :: a -> a -> a
    orE  :: a -> a -> a

-- modified Program instance:     
instance Expr Program where
    lit n     = [PushI n]
    add x y   = x ++ y ++ [Add]
    mul x y   = x ++ y ++ [Mul]
    bool b    = [PushB b]
    andE x y  = x ++ y ++ [And]
    orE x y   = x ++ y ++ [Or]
-}


-- Ex. 6: stored values
class HasVars a where
    var :: String -> a

data VarExprT = VetLit Integer
            | VetAdd VarExprT VarExprT
            | VetMul VarExprT VarExprT
            | VetVar String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = VetLit
    add = VetAdd
    mul = VetMul

-- I liked the idea of wrapping the map type up, stolen from here:
-- https://github.com/OctaviPascual/cis194-IntroductionToHaskell/blob/master/homework-05/Calc.hs
type MapSI = M.Map String Integer

instance HasVars VarExprT where
    var = VetVar

{-
instance Expr (MapSI -> Maybe Integer) where
    lit n _ = Just n
    add f g m = fmap (+) (f m) <*> g m
    mul f g m = fmap (*) (f m) <*> g m
-}

-- not sure for now if it makes sense, 
-- but while writing the instance I thought about the use of a third expression and I found out about liftA2,
--so I came out with another version...

-- helper function to lift a binary operation over two applicative functors
combiner :: Applicative f => (a -> b -> c) -> (m -> f a) -> (m -> f b) -> m -> f c
combiner op f g m = liftA2 op (f m) (g m)

instance Expr (MapSI -> Maybe Integer) where
    lit n _ = Just n
    add = combiner (+)
    mul = combiner (*)

instance HasVars (MapSI -> Maybe Integer) where
    var = M.lookup

withVars :: [(String, Integer)] -> (MapSI -> Maybe Integer) -> Maybe Integer
withVars vars expr = expr $ M.fromList vars

