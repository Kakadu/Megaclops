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

data State = State (Vector (Vector CellInfo))

instance Show State where
  show (State a) = concatMap (\x -> (f x) ++ "\n") (toList a)
    where f arr = concatMap show  (toList arr)
          
mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f  = (map (\(i,x) -> f i x)) . (zip [0..])
mapi2 :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
mapi2 f = mapi (\ i -> mapi (\j -> f i j)) 
      

changeState :: (Int, (Int, Int))  -> State -> State
changeState (playerNum, (x, y)) (State st) = 
  let helper xc yc item = PlayerKlop playerNum in
  State (st // [(x, (st ! x) // [ ( y,helper x y (st ! x ! y)) ]) ] )
  
-- gameLoop playerNumber movesCount curField
--
--gameLoop :: Int -> Int -> State -> _
gameLoop playerNum movesN st =
  do    
    putStrLn (show st)
    printf "Hi, Player %d! You can put %d klops now\n" (playerNum :: Int) (movesN :: Int)
    cmd <- getLine    
    let s2i = \x -> read x :: Int
    let z = map ((\x -> (playerNum, (s2i $ x!!0, s2i $  x!!1)) ) . (take 2) . (split ",")) (words cmd) 
    applyMove st z
    where
      applyMove st z | length z > movesN = 
        do
          printf "You should write not more %d cells in the line" (movesN :: Int)
          gameLoop playerNum 2 st
      applyMove st z | length z == movesN = 
        gameLoop (mod (playerNum + 1) 2) 5 (foldl (\st x -> changeState x st) st z)
      applyMove st z  = gameLoop playerNum (movesN - (length z)) (foldl (\st x -> changeState x st) st z)

emptyField :: State
emptyField = 
  State (V.generate k (const $ row ()) )
  where row () = V.generate n (const init) 
        -- I'm not sure that without function all values will be different
        init = Empty
        n = 20
        k = 20
  
          
exeMain = do
    putStrLn "Starting a game"
    gameLoop 0 5 emptyField

main = exeMain

