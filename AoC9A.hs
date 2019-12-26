module AoC9A (runIntcode) where

import Data.List.Split (splitOn)
import Data.List (replicate)

{-
1 add
2 multiply
3 take input
4 give output
5 jump if not eq
6 jump if eq
7 write whether less
8 write whether eq
9 adjust relative base
-}

replace :: Int -> Int -> [Int] -> [Int]
replace pos el lis = (++) (take pos lis) $ el : (drop (pos+1) lis)

write :: Int -> Int -> [Int] -> [Int]
write pos el lis = if pos < len
    then replace pos el lis
    else (++) lis $ reverse $ el : (replicate (pos - len) 0)
    where
        len = length lis

value :: Int -> Int -> Int -> [Int] -> Int
value mode oper base prog = case mode of
    0 -> if oper < length prog then prog !! oper else 0
    1 -> oper
    2 -> if pos < length prog then prog !! pos else 0
    where
        pos = base + oper

runIntcode :: Int -> [Int] -> [Int] -> Int -> [Int] -> [Int]
runIntcode pos ins outs base prog = case instr of 
    1 -> runIntcode (pos+4) ins outs base $ write tpos (fval + sval) prog
    2 -> runIntcode (pos+4) ins outs base $ write tpos (fval * sval) prog
    3 -> runIntcode (pos+2) (tail ins) outs base $ write fpos (head ins) prog
    4 -> runIntcode (pos+2) ins (fval:outs) base prog
    5 -> runIntcode (if fval /= 0 then sval else pos+3) ins outs base prog
    6 -> runIntcode (if fval == 0 then sval else pos+3) ins outs base prog
    7 -> runIntcode (pos+4) ins outs base $ write tpos (if fval < sval then 1 else 0) prog
    8 -> runIntcode (pos+4) ins outs base $ write tpos (if fval == sval then 1 else 0) prog
    9 -> runIntcode (pos+2) ins outs (base + fval) prog
    99 -> reverse outs
    where
        spl = drop pos prog
        opcode = head spl
        instr = opcode `mod` 100
        fmod = (opcode `div` 100) `mod` 10
        smod = (opcode `div` 1000) `mod` 10
        tmod = (opcode `div` 10000) `mod` 10
        foper = spl !! 1
        soper = spl !! 2
        toper = spl !! 3
        fval = value fmod foper base prog
        sval = value smod soper base prog
        fpos = if fmod == 2 then base + foper else foper
        tpos = if tmod == 2 then base + toper else toper

main = do
    fmap (
        (runIntcode 0 [1] [] 0) .
        map read .
        (splitOn ",")) $ readFile "9.txt"
    >>= print