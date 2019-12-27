import Text.Regex.PCRE ((=~))

totalEnergy :: [([Int], [Int])] -> Int
totalEnergy = sum . (map $ \(p,v) -> (sum $ map abs p) * (sum $ map abs v))

gravity :: [Int] -> [Int] -> [Int]
gravity sub obj = map (signum . (uncurry (-))) $ zip obj sub

simulate :: [[Int]] -> [[Int]] -> Int -> [([Int], [Int])]
simulate pos vels 0 = zip pos vels
simulate pos vels time =
    let 
        gravDeltas = map (\x -> foldl1 (zipWith (+)) $ map (gravity x) pos) pos
        newVels = zipWith (zipWith (+)) vels gravDeltas
        newPos = zipWith (zipWith (+)) pos newVels
    in simulate newPos newVels (time-1)

parsePositions :: String -> [Int]
parsePositions = map (read . (flip (!!) 1)) . flip (=~) "[xyz]=(-?\\d+)"

main = do
    fmap (
        totalEnergy .
        (\x -> simulate x (replicate (length x) [0,0,0]) 1000) .
        map parsePositions .
        lines) $ readFile "12.txt"
    >>= print