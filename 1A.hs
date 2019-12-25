calcFuel :: Int -> Int
calcFuel = (flip (-) 2) . (`div` 3)

main = do
    fmap (
        sum .
        map (calcFuel . read) .
        lines) $ readFile "1.txt"
    >>= print