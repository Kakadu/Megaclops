module Path (check) where

import Data.Vector

fetch :: (Vector (Vector a)) -> (Int, Int) -> a
fetch arr (x,y) = 
  arr ! x ! y
  
-- check arr startCell ok? checks if there is a path from starCell to another Cell for which 
-- ok? return Nothing. If ok? says Just lst than we should continue search in this list of 
-- cells too.
check :: (Vector (Vector a)) -> (Int,Int) -> (a -> Maybe [(Int,Int)]) -> Bool
check _ _ _ = 
  -- we need a container for visited cells
  -- we will use helper function and accumulator for cells
  True

