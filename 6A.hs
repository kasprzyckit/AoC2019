import Data.List.Split (splitOn)

orbits :: String -> Int -> [(String, String)] -> Int
orbits obj pred orbs =
    let
        children = filter ((==obj) . fst) orbs
        childOrbits = map (\(_,o) -> orbits o (pred+1) orbs) children
    in pred + sum childOrbits

main = do
    fmap (
        orbits "COM" 0 .
        map (\(x:y:_) -> (x, y)) .
        map (splitOn ")") .
        lines) $ readFile "6.txt"
    >>= print