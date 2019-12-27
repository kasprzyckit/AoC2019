import Text.Regex.PCRE ((=~))
import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)

findPeriod :: Int -> [Int] -> Int -> Int
findPeriod len ser per
    | (2*per) > len                             = error "Period not found"
    | take per ser == take per (drop per ser)   = per
    | otherwise                                 = findPeriod len ser (per+1)

gravity :: [Int] -> [Int] -> [Int]
gravity sub obj = map (signum . (uncurry (-))) $ zip obj sub

simulate :: [[Int]] -> [[Int]] -> Int -> [[Int]] -> [[Int]]
simulate pos vels 0 h = (concat $ map concat [pos, vels]) : h
simulate pos vels time h =
    let 
        gravDeltas = map (\x -> foldl1 (zipWith (+)) $ map (gravity x) pos) pos
        newVels = zipWith (zipWith (+)) vels gravDeltas
        newPos = zipWith (zipWith (+)) pos newVels
    in simulate newPos newVels (time-1) ((concat $ map concat [pos, vels]):h)

parsePositions :: String -> [Int]
parsePositions = map (read . (flip (!!) 1)) . flip (=~) "[xyz]=(-?\\d+)"

main = do
    fmap (
        foldl1 lcm .
        map (\x -> findPeriod (length x) x 1) .
        transpose .
        (\x -> simulate x (replicate (length x) [0,0,0]) 400000 []) .
        map parsePositions .
        lines) $ readFile "12.txt"
    >>= print
