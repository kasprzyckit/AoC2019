import AoC11A (paint)
import Prelude hiding (lookup)
import Data.Char (intToDigit)
import Data.Map (Map, fromList, keys, mapKeys, lookup)
import Data.List.Split (splitOn, chunksOf)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

printId :: Map (Int, Int) Int -> (Int, Int) -> String
printId m (width, height) =
    let pos = [(x,y) | y <- [0..(height-1)], x <- [0..(width-1)]]
        pixs = map ((fromMaybe 0) . (flip lookup m)) pos
    in intercalate "\n" $ chunksOf width $ map (\x -> if x == 0 then '-' else 'O') pixs

normalizeMap :: Map (Int, Int) Int -> (Map (Int, Int) Int, (Int, Int))
normalizeMap m =
    let ks = keys m
        maxW = maximum $ map fst ks
        minW = minimum $ map fst ks
        maxH = maximum $ map snd ks
        minH = minimum $ map snd ks
        width = maxW - minW + 1
        height = maxH - minH + 1
    in (mapKeys (\(x,y) -> (x-minW,y-minH)) m, (width, height))

panelsPainted :: [Int] -> Map (Int, Int) Int
panelsPainted prog = paint (0, [], [], 0, prog) (fromList [((0,0),1)]) (0, 0) 0

main = do
    fmap (
        uncurry printId .
        normalizeMap .
        panelsPainted .
        map read .
        (splitOn ",")) $ readFile "11.txt"
    >>= putStrLn