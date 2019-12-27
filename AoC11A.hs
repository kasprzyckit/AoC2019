module AoC11A (paint) where

import AoC9B (runIntcode, IntcodeState)
import Data.Map (Map, member, keys, empty, insert, (!))
import Data.List.Split (splitOn)

move :: (Int, Int) -> Int -> (Int, Int)
move (x,y) dir = case dir of 
    0 -> (x, y+1)
    1 -> (x+1, y)
    2 -> (x, y-1)
    3 -> (x-1, y)

paint :: IntcodeState -> Map (Int, Int) Int -> (Int, Int) -> Int -> Map (Int, Int) Int
paint (p,_,_,b,pr) m pos@(x,y) dir =
    let curCol = if member pos m then m ! pos else 0
        (c, (p2,_,out,b2,pr2)) = runIntcode p [curCol] [] b pr
        (newCol:turn:_) = out
        newDir = flip mod 4 $ if turn == 0 then dir-1 else dir+1
        newPos = move pos newDir
        newM = insert pos newCol m
    in if c == 99 then m else paint (p2,[],[],b2,pr2) newM newPos newDir

panelsPainted :: [Int] -> [(Int, Int)]
panelsPainted prog = keys $ paint (0, [], [], 0, prog) empty (0, 0) 0

main = do
    fmap (
        length .
        panelsPainted .
        map read .
        (splitOn ",")) $ readFile "11.txt"
    >>= print