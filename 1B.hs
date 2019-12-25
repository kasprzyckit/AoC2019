calcFuel :: Int -> Int
calcFuel =
    let calcFuel' total 0 = total
        calcFuel' total mass = calcFuel' (total + fuel) fuel
            where fuel = max 0 $ (flip (-) 2) . (`div` 3) $ mass
    in calcFuel' 0


main = do
    fmap (
        sum .
        map (calcFuel . read) .
        lines) $ readFile "1.txt"
    >>= print