import Data.List.Split (splitOn)
import Data.List (intersect, nub)

manhattan :: (Int, Int) -> Int
manhattan (x, y) = (abs x) + (abs y)

findCommon :: [[(Int, Int)]] -> [(Int, Int)]
findCommon (l1:l2:_) = nub $ intersect (nub l1) (nub l2)

pathV :: [(Int, Int)] -> (Int, Int) -> [Int] -> [(Int, Int)]
pathV acc _ [] = acc
pathV acc (x, y) (dir:tail) =
    let
        newY = y + dir
        str = min y newY
        end = max y newY
        pnts = filter (/= (x, y)) [(x, yy) | yy <- [str..end]]
    in pathH (acc ++ pnts) (x, newY) tail

pathH :: [(Int, Int)] -> (Int, Int) -> [Int] -> [(Int, Int)]
pathH acc _ [] = acc
pathH acc (x, y) (dir:tail) =
    let
        newX = x + dir
        str = min x newX
        end = max x newX
        pnts = filter (/= (x, y)) [(xx, y) | xx <- [str..end]]
    in pathV (acc ++ pnts) (newX, y) tail

path :: [Int] -> [(Int, Int)]
path = pathH [] (0, 0)

readDirection :: String -> Int
readDirection (x:tail) = case x of
    'D' -> (-1) * num
    'L' -> (-1) * num
    'U' -> num
    'R' -> num
    where num = read tail

main = do
    fmap (
        minimum .
        map manhattan .
        findCommon .
        map path .
        map (map readDirection) .
        map (splitOn ",") .
        lines) $ readFile "3.txt"
    >>= print