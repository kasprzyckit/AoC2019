import Data.List.Split (splitOn)

replace :: Int -> Int -> [Int] -> [Int]
replace pos el lis = (++) (take pos lis) $ el : (drop (pos+1) lis)

runIntcode :: Int -> [Int] -> [Int]
runIntcode pos prog = case opcode of 
    1 -> runIntcode (pos+4) $ replace toper (fval + sval) prog
    2 -> runIntcode (pos+4) $ replace toper (fval * sval) prog
    99 -> prog
    where
        spl = drop pos prog
        opcode = head spl
        foper = spl !! 1
        soper = spl !! 2
        toper = spl !! 3
        fval = prog !! foper
        sval = prog !! soper

main = do
    fmap (
        (runIntcode 0) .
        map read .
        (splitOn ",")) $ readFile "2.txt"
    >>= print