import Data.List.Split (splitOn)
import Data.List (findIndex)

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

output :: (Int, Int) -> [Int] -> Int
output (noun, verb) prog =
    let mem = (head prog) : noun : verb : (drop 3 prog)
    in head $ runIntcode 0 mem

findCorrect :: [Int] -> Maybe Int
findCorrect prog =
    let
        comb = [(x, y) | x<-[0..99], y<-[0..99]]
        indx = findIndex (\x -> output x prog == 19690720) comb
    in fmap ((\(x, y) -> 100*x + y) . (comb !!)) indx

main = do
    fmap (
        findCorrect .
        map read .
        (splitOn ",")) $ readFile "2.txt"
    >>= print