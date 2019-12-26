import Data.List.Split (splitOn)
import Data.List (permutations)

type IntcodeState = (Int, [Int], [Int], [Int])

replace :: Int -> Int -> [Int] -> [Int]
replace pos el lis = (++) (take pos lis) $ el : (drop (pos+1) lis)

runIntcode :: Int -> [Int] -> [Int] -> [Int] -> (Int, IntcodeState)
runIntcode pos ins outs prog = case instr of 
    1 -> runIntcode (pos+4) ins outs $ replace toper (fval + sval) prog
    2 -> runIntcode (pos+4) ins outs $ replace toper (fval * sval) prog
    3 -> if null ins
        then (3, (pos, ins, reverse outs, prog))
        else runIntcode (pos+2) (tail ins) outs $ replace foper (head ins) prog
    4 -> runIntcode (pos+2) ins (fval:outs) prog
    5 -> runIntcode (if fval /= 0 then sval else pos+3) ins outs prog
    6 -> runIntcode (if fval == 0 then sval else pos+3) ins outs prog
    7 -> runIntcode (pos+4) ins outs $ replace toper (if fval < sval then 1 else 0) prog
    8 -> runIntcode (pos+4) ins outs $ replace toper (if fval == sval then 1 else 0) prog
    99 -> (99, (pos, ins, reverse outs, prog))
    where
        spl = drop pos prog
        opcode = head spl
        instr = opcode `mod` 100
        fmod = (opcode `div` 100) `mod` 10
        smod = (opcode `div` 1000) `mod` 10
        foper = spl !! 1
        soper = spl !! 2
        toper = spl !! 3
        fval = if fmod == 0 then prog !! foper else foper
        sval = if smod == 0 then prog !! soper else soper

singleThrust :: [Int] -> [IntcodeState] -> (Int, [Int], [IntcodeState])
singleThrust inp = foldl (\(_,y,z) (p,_,_,pr) ->
    let (c, st@(_,_,out,_)) = runIntcode p y [] pr
    in (c, out, st:z)) (0, inp, [])

thrustLoop :: [Int] -> [IntcodeState] -> [Int]
thrustLoop inp ints = case c of
    3   -> thrustLoop out $ reverse sts
    99  -> out
    where (c, out, sts) = singleThrust inp ints

thrust :: [Int] -> [Int] -> [Int]
thrust prog = (thrustLoop [0]) . (map (\x -> snd $ runIntcode 0 [x] [] prog))

findMaxThrust :: [Int] -> Int
findMaxThrust prog = maximum $ map (head . (thrust prog)) $ permutations [5..9]

main = do
    fmap (
        findMaxThrust .
        map read .
        (splitOn ",")) $ readFile "7.txt"
    >>= print