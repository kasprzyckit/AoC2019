import Data.List.Split (splitOn)
import Data.List (sort, nub)

meetsPred :: Int -> Bool
meetsPred n =
    let org = show n
        sorted = sort org
        dupl = nub org
    in org == sorted && length org > length dupl

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