module Path (check) where

import Data.Vector hiding (concatMap, filter)
import Data.Set as S hiding (filter)

--fetch :: (Vector (Vector a)) -> (Int, Int) -> a
--fetch arr (x,y) = arr ! x ! y
  
type Point = (Int,Int)  
-- check arr startCell neighbours ok checks if there is a path from starCell to another Cell for which 
-- ok return True. If ok says False  than we should continue search in [neighbours cell] too.
check :: Point -> (Point -> [Point]) -> (Point -> Bool) -> Bool
check init nei ok = 
  -- we need a container for visited cells
  -- we will use helper function and accumulator for cells
  helper (S.singleton init) [init]
  where 
    startSet = Prelude.foldl (\acc x -> S.insert x acc) S.empty startNei
    startNei = nei init
    helper :: Set Point -> [Point] -> Bool
    helper visited cells = 
      -- here all cells in 'cells' are evaluated
      let nextGen = filter (\x -> not (S.member x visited)) ( concatMap nei cells )
          (newVisited,ans) = Prelude.foldl folder (visited,False) nextGen
          folder :: (Set Point,Bool) -> Point -> (Set Point, Bool)
          folder (v,ans) _ | ans  = (v,ans)
          folder (v,___) e | ok e = (S.insert e v,True)
          folder (v,___) e        = (S.insert e v,False) in
      case ans of
        True -> True
        False -> helper newVisited nextGen

