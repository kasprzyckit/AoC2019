import Data.List.Split (splitOn)
import Data.List (sort, nub)

checkDupl :: String -> Bool
checkDupl [] = False
checkDupl (a:[]) = False
checkDupl (a:b:[]) = a == b
checkDupl (a:b:c:tail)
    | a == b && b == c  = checkDupl $ dropWhile (==c) tail
    | a == b && b /= c  = True
    | otherwise         = checkDupl (b:c:tail)

meetsPred :: Int -> Bool
meetsPred n =
    let org = show n
        sorted = sort org
    in org == sorted && checkDupl org

password :: Int -> Int -> Int -> Int
password count upperBound n
    | n > upperBound    = count
    | not (meetsPred n) = password count upperBound (n+1)
    | otherwise         = password (count+1) upperBound (n+1)

main = do
    fmap (
        (\(x:y:_) -> password 0 y x) .
        map read .
        splitOn "-") $ readFile "4.txt"
    >>= print