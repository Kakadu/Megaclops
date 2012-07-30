{-# LANGUAGE CPP, TemplateHaskell #-}

module Main (
    main
) where

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
      

changeState :: (CellInfo, (Int, Int))  -> State -> State
changeState (ci, (x, y)) (State st) = 
  let helper xc yc item = ci in
  State (st // [(x, (st ! x) // [ ( y,helper x y ci) ]) ] )
  

gameLoop playerNum st =
  do    
    putStrLn (show st)        
    cmd <- getLine    
    let s2i = \x -> read x :: Int
    let z = map ((\x -> (PlayerKlop playerNum, (s2i $ x!!0, s2i $  x!!1)) ) . (take 2) . (split ",")) (words cmd) 
    applyMove st z
    where
      applyMove st z | length z /= 2 = 
        do
          putStrLn ("You should write 2 cells in the line")
          gameLoop playerNum st
      applyMove st z = gameLoop (mod (playerNum + 1) 2) (foldl (\st x -> changeState x st) st z)

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
    gameLoop 0 emptyField

main = exeMain

