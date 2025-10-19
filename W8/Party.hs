{-# OPTIONS_GHC -Wall #-}

module Party where

import Employee
import Data.Tree
import Data.List (sortOn)

-- https://github.com/byorgey/haskell-course

-- https://github.com/OctaviPascual/cis194-IntroductionToHaskell
-- https://github.com/severij/cis194-solutions/tree/master
-- https://github.com/bschwb/cis194-solutions
-- https://github.com/baugarten/CIS-194
-- https://github.com/surganov/cis194

-- Ex.1: basic tools

-- add an employee on the guest list without checks
-- I kept it a little verbose for clarity
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL oldList oldFun) = GL (emp : oldList) (oldFun + empFun emp)

-- moved the instance for GuestList inside Employee.hs

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Ex. 2: folding roses (a little verbose again)
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root subForest) = f root (map (treeFold f) subForest)

-- Ex. 3: with and without bosses (trying to use treeFold)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subResults =
  let withBoss = glCons boss (mconcat (map snd subResults))
      withoutBoss = mconcat (map (uncurry moreFun) subResults)
  in (withBoss, withoutBoss)

-- Ex. 4: putting it all together
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Ex.5: testing the code against the whole company
sortEmployees :: [Employee] -> String
sortEmployees emps = unlines (map empName (sortOn empName emps))

formatGuestList :: GuestList -> String
formatGuestList (GL emps fun) = "Total fun: " ++ show fun ++ "\n" ++  sortEmployees emps

main :: IO ()
main = do
  contents <- readFile "company.txt"
  let companyTree = read contents :: Tree Employee
      bestGuestList = maxFun companyTree
  putStrLn (formatGuestList bestGuestList)
