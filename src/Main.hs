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

data State = State [[CellInfo]] 

instance Show State where
  show (State lst) = concatMap (\x -> (f x) ++ "\n") lst
    where f     =  concatMap  $ show              
mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f  = (map (\(i,x) -> f i x)) . (zip [0..])
mapi2 :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
mapi2 f = mapi (\ i -> mapi (\j -> f i j)) 
      

changeState :: (CellInfo, (Int, Int))  -> State -> State
changeState (ci, (x, y)) (State st) = 
  let helper xc yc item = ci in
  State (mapi2 (\xc yc item -> if (xc == x && yc == y) then helper xc yc item else item) st)
  

gameLoop playerNum st =
  do    
    putStrLn (show st)        
    cmd <- getLine    
    let s2i = \x -> read x :: Int
    let z = map ((\x -> (PlayerKlop playerNum, (s2i $ x!!0, s2i $  x!!1)) ) . (take 2) . (split ",")) (words cmd) 
    -- putStrLn $ show z
    gameLoop (mod (playerNum + 1) 2) (foldl (\st x -> changeState x st) st z)

emptyField :: State
emptyField = 
  State $ map (const $ map (const Empty) [1..n]) [1..n]
    where n = 20
          
exeMain = do
    --s <- getLin
    putStrLn "Starting a game"
    gameLoop 0 emptyField

main = exeMain

