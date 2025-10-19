{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where
    
import Data.Char
-- The following import should be really needed only using the version of scoreString with foldMap
-- import Data.Monoid

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty  = Score 0

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

score :: Char -> Score
score c
  | lc `elem` "aeilnorstu" = Score 1
  | lc `elem` "dg"         = Score 2
  | lc `elem` "bcmp"       = Score 3
  | lc `elem` "fhvwy"      = Score 4
  | lc `elem` "k"          = Score 5
  | lc `elem` "jx"         = Score 8
  | lc `elem` "qz"         = Score 10
  | otherwise              = Score 0
    where lc = toLower c

scoreString :: String -> Score
scoreString = foldr ((+) . score) (Score 0)
-- scoreString = foldMap score

getScore :: Score -> Int
getScore (Score s) = s