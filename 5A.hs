import Data.List.Split (splitOn)

replace :: Int -> Int -> [Int] -> [Int]
replace pos el lis = (++) (take pos lis) $ el : (drop (pos+1) lis)

runIntcode :: Int -> [Int] -> [Int] -> [Int] -> [Int]
runIntcode pos ins outs prog = case instr of 
    1 -> runIntcode (pos+4) ins outs $ replace toper (fval + sval) prog
    2 -> runIntcode (pos+4) ins outs $ replace toper (fval * sval) prog
    3 -> runIntcode (pos+2) (tail ins) outs $ replace foper (head ins) prog
    4 -> runIntcode (pos+2) ins (fval:outs) prog
    99 -> reverse outs
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

main = do
    fmap (
        (runIntcode 0 [1] []) .
        map read .
        (splitOn ",")) $ readFile "5.txt"
    >>= print