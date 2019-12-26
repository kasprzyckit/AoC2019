import Data.Char (digitToInt, intToDigit)
import Data.List.Split (chunksOf)
import Data.List (intercalate)

height = 6
width = 25

stackPixels :: (Int, Int) -> Int
stackPixels (a, b) = if a == 2 then b else a

stackLayers :: [Int] -> [Int] -> [Int]
stackLayers a b = map stackPixels $ zip a b

main = do
    fmap (
        intercalate "\n" .
        chunksOf width .
        map (\x -> if x == 0 then '-' else 'O') .
        foldl1 stackLayers .
        chunksOf (height * width) .
        map digitToInt) $ readFile "8.txt"
    >>= putStrLn