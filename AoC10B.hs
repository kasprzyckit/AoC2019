import Data.Map (Map, fromList, member)
import Data.List (intersect, (\\), maximumBy, sortBy)
import Data.Function (on)

sortWith :: Ord a => (b -> a) -> [b] -> [b]
sortWith f = sortBy (compare `on` f)

angle :: (Int, Int) -> (Int, Int) -> Double
angle (x1,y1) (x2,y2) =
    let
        xx = fromIntegral $ x2 - x1 :: Double
        yy = fromIntegral $ y2 - y1 :: Double
    in (+) 4 $ atan2 xx yy

sortPolar :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
sortPolar obs asts = reverse $ map fst $ sortWith snd $ map (\x -> (x, angle obs x)) asts

lastBatch :: (Int, Int) -> [(Int, Int)] -> Int -> ([(Int, Int)], Int)
lastBatch obs asts vapor =
    let
        lkM = fromList $ map (\x -> (x, True)) asts
        batch = filter (observable lkM obs) asts
        len = length batch
    in if vapor - len < 1 then (batch, vapor) else lastBatch obs (asts \\ batch) (vapor - len)

maximumWith :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maximumWith f = maximumBy (compare `on` f)

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

mostObserved :: [(Int, Int)] -> (Int, Int)
mostObserved asts =
    let lkM = fromList $ map (\x -> (x, True)) asts
        obs = map (\x -> (x, (length . (flip filter asts) . (observable lkM)) x)) asts
    in fst $ maximumWith snd obs

serializeLine :: (Int, String) -> [(Int, Int)]
serializeLine (l, s) = map (\x -> (snd x, l)) $ filter ((=='#') . fst) $ zip s [0..]

main = do
    fmap (
        (\(x,y) -> 100*x + y) .
        (\(o, (b, r)) -> (sortPolar o b) !! (r-1)) .
        (\(a,o) -> (o, lastBatch o a 200)) .
        (\x -> (x, mostObserved x)) .
        concat .
        map serializeLine .
        zip [0..] .
        lines) $ readFile "10.txt"
    >>= print