import Data.Map (Map, fromList, member)
import Data.List (intersect, (\\))

divisors :: Int -> [Int]
divisors n = (:) n $ concat [ [x, div n x] | x <- [2..limit], rem n x == 0]
     where limit = (floor . sqrt . fromIntegral) n

cord :: Int -> Int -> Int -> [Int]
cord di nn n1 =
    let 
        abnn = abs nn
        dn = abnn `div` di
    in map (((-) n1) . ((*) $ signum nn)) $ enumFromThenTo dn (2*dn) (abnn-1)

blocking :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
blocking a@(x1,y1) b@(x2,y2)
    | xx == 0       = zip [x1,x1..] $ (enumFromThenTo y2 ((+) y2 $ signum yy) y1) \\ [y1, y2]
    | yy == 0       = flip zip [y1,y1..] $ (enumFromThenTo x2 ((+) x2 $ signum xx) x1) \\ [x1, x2]
    | otherwise     = concat $ map (\k -> zip (cord k xx x1) (cord k yy y1)) comDiv
    where
        xx = x1 - x2
        yy = y1 - y2
        comDiv = intersect (divisors $ abs xx) (divisors $ abs yy)

observable :: Map (Int, Int) Bool -> (Int, Int) -> (Int, Int) -> Bool
observable lk obs ast = not $ any (flip member lk) $ blocking obs ast

countObservable :: [(Int, Int)] -> [Int]
countObservable asts =
    let lkM = fromList $ map (\x -> (x, True)) asts
    in map (length . (flip filter asts) . (observable lkM)) asts
        

serializeLine :: (Int, String) -> [(Int, Int)]
serializeLine (l, s) = map (\x -> (l, snd x)) $ filter ((=='#') . fst) $ zip s [0..]

main = do
    fmap (
        maximum .
        countObservable .
        concat .
        map serializeLine .
        zip [0..] .
        lines) $ readFile "10.txt"
    >>= print