import Data.Char (digitToInt)
import Data.List.Split (chunksOf)
import Util (minWith)

height = 6
width = 25

main = do
    fmap (
        (\l -> product $ map (\x -> length $ filter (==x) l) [1,2]) .
        minWith (length . (filter (==0))) .
        chunksOf (height * width) .
        map digitToInt) $ readFile "8.txt"
    >>= print