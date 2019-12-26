import AoC5B (runIntcode)
import Data.List.Split (splitOn)
import Data.List (permutations)

thrust :: [Int] -> [Int] -> [Int]
thrust prog = foldl (\x y -> runIntcode 0 (y:x) [] prog) [0]

findMaxThrust :: [Int] -> Int
findMaxThrust prog = maximum $ map (head . (thrust prog)) $ permutations [0..4]

main = do
    fmap (
        findMaxThrust .
        map read .
        (splitOn ",")) $ readFile "7.txt"
    >>= print