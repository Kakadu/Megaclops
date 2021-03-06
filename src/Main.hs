{-# LANGUAGE CPP, TemplateHaskell #-}

module Main (
    main
) where
import Text.Printf (printf)
import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import Data.String.Utils (split)
--import Control.Applicative 
import Control.Exception.Base (assert)
import Data.Vector as V hiding (concatMap, (++), map, zip, take, length, foldl)
import Path

data CellInfo = PlayerKlop Int
              | PlayerFruit Int
              | Empty 

instance Show CellInfo where                                
  show x = 
    case x of 
      PlayerKlop 0 -> "0"
      PlayerKlop 1 -> "x"
      PlayerFruit 0 -> "a"          
      PlayerFruit 1 -> "b"
      Empty -> "_"
      _ -> assert False "12439u12-u4=-123"

data State = State (Vector (Vector CellInfo)) [Int]

instance Show State where
  show (State a b) = concatMap (\x -> (f x) ++ "\n") (toList a)
                     ++ foldl (\buf (count, index) -> buf ++ "\nPlayer " ++ (show index) ++ ": " ++ (show count) ++ "klops") "" (zip b [0..])
    where f arr = concatMap show  (toList arr)
          
mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f  = (map (\(i,x) -> f i x)) . (zip [0..])
mapi2 :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
mapi2 f = mapi (\ i -> mapi (\j -> f i j))

changeAt :: Int -> a -> [a]-> [a]
changeAt index newValue list =
    if (index >= length list)
    then
        list
    else
        case (Prelude.splitAt index list) of
          (l, (_:xs)) -> l ++ (newValue:xs)
          _ -> list

changeState :: (Int, (Int, Int))  -> State -> State
changeState (playerNum, (x, y)) (State st counter) = 
  let helper xc yc item = PlayerKlop playerNum in
  State (st // [(x, (st ! x) // [ ( y,helper x y (st ! x ! y))])]) $ changeAt playerNum ((counter !! playerNum) + 1) counter
  
nei size (x,y) = 
  Prelude.filter (\(x,y) -> (check' x) && (check' y) ) [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]
  where check' x = (x>=0) && (x< size)
        
updateField st (x,y) newVal =
  st // [(x, (st ! x) // [(y,newVal)] ) ]  

-- evalMoves field playerN [points]   return either new state if all moves are OK
-- or Error message with count of evaluated moves
evalMoves :: State -> Int -> [(Int,Int)] -> Either (State,Int) (String,Int)
evalMoves st playerN cells =
  wrap $ foldl helper (Left (st,0)) cells
  where 
    wrap (Right x) = Right x
    wrap x = x
    goodCell st (x,y) = 
      case st ! x ! y of
        PlayerKlop x | x == playerN -> True
        otherwise  -> False
    placableCell st (x,y) = 
      case st ! x ! y of
        PlayerKlop pnum | pnum == playerN -> False
        PlayerFruit _ -> False
        otherwise -> True
    nei' st cell = Prelude.filter (nei'' st) $ nei 10 $ cell
    nei'' st (x,y) = 
      case st ! x ! y of
        PlayerKlop x | x == playerN -> True
        PlayerKlop _                -> False
        PlayerFruit x | x ==playerN -> True
        PlayerFruit _               -> False
        Empty                       -> False
    helper (Right x) _ = Right x
    helper (Left (State st counter,n)) cell | (placableCell st cell) && (Path.check cell (nei' st) (goodCell st)) = 
      Left (State (updateField st cell cellValue) 
                  updatedCounter
           ,n+1)
           where (cellValue, updatedCounter) = newCell st cell counter
    helper (Left (_,n)) _ = Right ("cant put klop", n)
    newCell st (x,y) counter =
      case st ! x ! y of
        Empty -> (PlayerKlop playerN , changeAt playerN ((counter !! playerN)+1) counter)
        PlayerFruit _ -> error "Impossible!"
        PlayerKlop x | x /= playerN ->  (PlayerFruit playerN, changeAt x ((counter !! x)-1) counter)
        _ -> error "Impossible2!"
      
  
-- gameLoop playerNumber movesCount curField
--
--gameLoop :: Int -> Int -> State -> _
gameLoop playerNum movesN st@(State _ cntr) =
  case Prelude.filter (\(item, num) -> item /= 0 ) $ zip cntr [0..] of
    [(_, pNum)] ->
      printf "Player %d won!!!\n" (pNum :: Int)
    _ ->
      if (cntr !! playerNum) == 0
      then
        do
          printf "Player %d is out\n" (playerNum :: Int)
          gameLoop (mod (playerNum + 1) 2) 5 st
      else
        do
          putStrLn (show st)
          printf "Hi, Player %d! You can put %d klops now\n" (playerNum :: Int) (movesN :: Int)
          cmd <- getLine
          case cmd of
            "skip" -> gameLoop (mod (playerNum + 1) 2) 5 st
            _ ->
              do
                let s2i = \x -> read x :: Int
                let z = map ((\x -> ( (s2i $ x!!0, s2i $  x!!1)) ) . (take 2) . (split ",")) (words cmd) 
                case length z of
                  x | x > movesN -> 
                    do 
                      printf "You should not write more than %d cells in a line" (movesN :: Int)
                      gameLoop playerNum 2 st
                  x -> doApplyMoves z
                    where
                      doApplyMoves z =
                        case evalMoves st playerNum z of
                          Left (st,count) | count == movesN ->
                            do
                              putStrLn "All moves were applied\n"
                              gameLoop (mod (playerNum + 1) 2) 5 st
                          Left (st, count) | count < movesN ->
                            do 
                              printf "%d  moves were applied. %d left\n" (count :: Int) ((movesN - count) :: Int)
                              gameLoop playerNum (movesN-count) st
                          Right (msg,count) ->
                            do
                              printf "Error after successful applying of %d moves: %s\n" (count :: Int) (msg :: String)
                              gameLoop playerNum movesN st

emptyField :: State
emptyField = 
  State ( empty // [(1, empty ! 1 // [ (1,PlayerKlop 1) ])] // [(8, empty ! 8 // [ (8,PlayerKlop 0) ])] ) [1, 1]
  where empty = V.generate k (const $ row ()) 
        row () = V.generate n (const init) 
        -- I'm not sure that without function all values will be different
        init = Empty
        n = 10
        k = 10
  
          
exeMain = do
    putStrLn "Starting a game"
    gameLoop 0 5 emptyField

main = exeMain

