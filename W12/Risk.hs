{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


-- Ex. 2: battle simulation

attack :: Army -> Int
attack n = min 3 (n-1)

defense :: Army -> Int
-- defense n = min 2 n
defense = min 2

rollDices :: Int -> Rand StdGen [DieValue]
rollDices n = replicateM n die

sortDices :: [DieValue] -> [DieValue]
sortDices = sortBy (comparing Data.Ord.Down)

compareDices :: [DieValue] -> [DieValue] -> [(DieValue, DieValue)]
compareDices = zip (sortDices attRolls) (sortDices defRolls)

updateLosses :: (Int, Int) -> (DieValue, DieValue) -> (Int, Int)
updateLosses (attLoss, defLoss) (attDie, defDie)
  | unDV attDie > unDV defDie = (attLoss, defLoss + 1)
  | otherwise = (attLoss + 1, defLoss)

countLosses :: [(DieValue, DieValue)] -> (Int, Int)
countLosses = foldl' updateLosses (0, 0)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield att def) = do
  let attDice = attack att
      defDice = defense def
  attRolls <- rollDices attDice
  defRolls <- rollDices defDice
  let dicePairs = compareDices attRolls defRolls
      (attLosses, defLosses) = countLosses dicePairs
  return $ Battlefield (att - attLosses) (def - defLosses)


-- Ex.3: invasion

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield att def)
  | att < 2 || def == 0 = return bf
  | otherwise = battle bf >>= invade

{-
I like this solution: https://github.com/OctaviPascual/cis194-IntroductionToHaskell/blob/master/homework-12/Risk.hs
I leave it here:

-- Returns whether or not an invasion has ended
hasInvasionEnded :: Battlefield -> Bool
hasInvasionEnded (Battlefield as ds) = ds == 0 || as < 2

-- Simulates an entire invasion attempt
invade :: Battlefield -> Rand StdGen Battlefield
invade = iterateUntilM hasInvasionEnded battle
-}

success :: [Battlefield] -> Rand StdGen Double
success invasion = return $ fromIntegral (length duration) / fromIntegral (length invasion)
  where duration = filter (\(Battlefield _ def) -> def == 0) invasion
  -- nice from bschwb:
  -- where duration = filter ((== 0) . defenders) invasion

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM 1000 (invade bf)
  success results
  
{-
using a pure version of success (not monadic):

success :: [Battlefield] -> Double
success invasion = fromIntegral (length duration) / fromIntegral (length invasion)
  where duration = filter ((== 0) . defenders) invasion


successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM 1000 (invade bf)
  return $ success results

-}


-- Ex. 5: probability
isBattleOver :: Battlefield -> Bool
isBattleOver (Battlefield att def) = att < 2 || def == 0

conclusionProb :: Battlefield -> Double
conclusionProb (Battlefield _ def)
  | def == 0  = 1.0
  | otherwise = 0.0

diceCounts :: Battlefield -> (Int, Int)
diceCounts (Battlefield att def) = (min 3 (att - 1), min 2 def)

allDiceRolls :: Int -> [[Int]]
allDiceRolls n = replicateM n [1..6]

sortDice :: [Int] -> [Int]
sortDice = sortBy (comparing Down)

computeLosses :: [Int] -> [Int] -> (Int, Int)
computeLosses attRolls defRolls = foldl' count (0, 0) (zip (sortDice attRolls) (sortDice defRolls))
  where
    count (attLoss, defLoss) (att, def)
      | att > def = (attLoss, defLoss + 1)
      | otherwise = (attLoss + 1, defLoss)

nextBattlefield :: Battlefield -> [Int] -> [Int] -> Battlefield
nextBattlefield (Battlefield att def) attRolls defRolls =
  let (attLoss, defLoss) = computeLosses attRolls defRolls
  in Battlefield (att - attLoss) (def - defLoss)

exactSuccessProb :: Battlefield -> Double
exactSuccessProb bf
  | isBattleOver bf = conclusionProb bf
  | otherwise     =
      let (attDice, defDice) = diceCounts bf
          attRollsList = allDiceRolls attDice
          defRollsList = allDiceRolls defDice
          attCount = length attRollsList
          defCount = length defRollsList
          totalCombos = fromIntegral (attCount * defCount)
          outcomeProbs = [ exactSuccessProb (nextBattlefield bf att def)
                         | att <- attRollsList, def <- defRollsList ]
      in sum outcomeProbs / totalCombos

