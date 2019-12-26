import Data.List.Split (splitOn)
import Data.List (intersect, nub, findIndex)
import Data.Maybe (fromMaybe)

type PathInfo = ([(Int, Int)], (Int, Int), [Int])

findCommon :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
findCommon l1 l2 = nub $ intersect (nub l1) (nub l2)

removeLoop :: (Int, Int) -> [(Int, Int)] -> Maybe [(Int, Int)]
removeLoop h l =
    let len = length l
        r = reverse r
        lastIndex = findIndex (==h) $ reverse l
    in fmap ((flip drop l) . (\x -> len-x-1)) lastIndex

shortenPathLen :: [(Int, Int)] -> Int
shortenPathLen =
    let shl acc [] = acc
        shl acc (h:tail) = shl (1 + acc) $ fromMaybe tail $ removeLoop h tail
    in shl 0

extractMinPath :: ([(Int, Int)], [(Int, Int)]) -> (Int, Int) -> Int
extractMinPath (path1, path2) pos =
    let
        p1 = length $ takeWhile (/= pos) path1
        p2 = length $ takeWhile (/= pos) path2
    in p1 + p2 + 2

pathV :: PathInfo -> PathInfo
pathV (acc, _, []) = (acc, (0, 0), [])
pathV (acc, (x, y), (dir:tail)) =
    let
        newY = y + dir
        str = min y newY
        end = max y newY
        pnts = filter (/= (x, y)) [(x, yy) | yy <- [str..end]]
        pntsDir = if y < newY then pnts else reverse pnts
    in ((acc ++ pntsDir), (x, newY), tail)

pathH :: PathInfo -> PathInfo
pathH (acc, _, []) = (acc, (0, 0), [])
pathH (acc, (x, y), (dir:tail)) =
    let
        newX = x + dir
        str = min x newX
        end = max x newX
        pnts = filter (/= (x, y)) [(xx, y) | xx <- [str..end]]
        pntsDir = if x < newX then pnts else reverse pnts
    in ((acc ++ pntsDir), (newX, y), tail)

shouldStop :: [a] -> [a] -> [b] -> [b] -> [c] -> Int -> Bool
shouldStop a1 a2 d1 d2 ints minP =
    not (null d1 || null d2) && (null ints || (length a1) < minP || (length a2) < minP)


intersV :: (PathInfo, PathInfo) -> Int
intersV ((a1, p1, d1), (a2, p2, d2)) =
    let ints = findCommon a1 a2
        minP = minimum $ map (extractMinPath (a1, a2)) ints
        pi1 = pathV (a1, p1, d1)
        pi2 = pathV (a2, p2, d2)
    in if shouldStop a1 a2 d1 d2 ints minP then intersH (pi1, pi2) else minP

intersH :: (PathInfo, PathInfo) -> Int
intersH ((a1, p1, d1), (a2, p2, d2)) =
    let ints = findCommon a1 a2
        minP = minimum $ map (extractMinPath (a1, a2)) ints
        pi1 = pathH (a1, p1, d1)
        pi2 = pathH (a2, p2, d2)
    in if shouldStop a1 a2 d1 d2 ints minP then intersV (pi1, pi2) else minP

findIntersection :: ([Int], [Int]) -> Int
findIntersection (d1, d2) = intersH (([], (0, 0), d1), ([], (0, 0), d2))

readDirection :: String -> Int
readDirection (x:tail) = case x of
    'D' -> (-1) * num
    'L' -> (-1) * num
    'U' -> num
    'R' -> num
    where num = read tail

main = do
    fmap (
        findIntersection .
        (\(l1:l2:_) -> (l1, l2)) .
        map (map readDirection) .
        map (splitOn ",") .
        lines) $ readFile "3.txt"
    >>= print