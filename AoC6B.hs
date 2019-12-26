import Data.List.Split (splitOn)
import Data.List (find)

predecessors :: String -> [(String, String)] -> [String]
predecessors nod orbs = case find ((==nod) . snd) orbs of
    Nothing          -> []
    Just (parent, _) -> (:) parent $ predecessors parent orbs

distance :: String -> String -> [(String, String)] -> Int
distance nod1 nod2 orbs =
    let
        path1 = reverse $ predecessors nod1 orbs
        path2 = reverse $ predecessors nod2 orbs
        common = takeWhile (uncurry (==)) $ zip path1 path2
    in (length path1) + (length path2) - 2*(length common)

main = do
    fmap (
        distance "YOU" "SAN" .
        map (\(x:y:_) -> (x, y)) .
        map (splitOn ")") .
        lines) $ readFile "6.txt"
    >>= print