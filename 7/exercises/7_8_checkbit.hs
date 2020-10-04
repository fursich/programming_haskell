
type Bit = Int

addCheckBit :: [[Bit]] -> [[Bit]]
addCheckBit = map (\xs -> xs ++ [sum' xs `mod` 2] )

sum' :: [Bit] -> Int
sum' = foldl (+) 0

verifyCheckBit :: [[Bit]] -> [[Bit]]
verifyCheckBit = map (\xs -> if last xs == (sum (init xs) `mod` 2) then init xs else error $ "invalid check sum: " ++ show(xs) )

channel :: [[Bit]] -> [[Bit]]
channel = map tail

main = do
    putStrLn $ "addCheckBit [[0,0,1,0,0,0,1,0], [0,1,0,0,1,1,0,1], [1,1,1,0,1,1,1,0]] = " ++
                show ( addCheckBit [[0,0,1,0,0,0,1,0], [0,1,0,0,1,1,0,1], [1,1,1,0,1,1,1,0]] )

    putStrLn $ "addCheckBit [[1,0,0,0,0,1,0,0], [0,1,0,0,0,1,1,1], [1,0,0,0,0,1,1,0]] = " ++
                show ( addCheckBit [[1,0,0,0,0,1,0,0], [0,1,0,0,0,1,1,1], [1,0,0,0,0,1,1,0]] )

    putStrLn $ "verifyCheckBit [[1,0,0,0,0,1,1,0,1], [0,1,0,0,0,1,1,0,1], [1,1,0,0,0,1,1,0,0]] = " ++
                show ( verifyCheckBit [[1,0,0,0,0,1,1,0,1], [0,1,0,0,0,1,1,0,1], [1,1,0,0,0,1,1,0,0]] )

--    putStrLn $ "verifyCheckBit [[1,1,0,0,0,0,1,1,0], [1,0,1,0,0,0,1,1,0], [1,1,1,0,0,0,1,1,0]] = " ++
--                show ( verifyCheckBit [[1,1,0,0,0,0,1,1,0], [1,0,1,0,0,0,1,1,0], [1,1,1,0,0,0,1,1,0]] )

    putStrLn $ "verifyCheckBit ( channel ( addCheckBit [[0,1,0,1,0,1,0,1], [0,0,0,0,1,1,0,0]]) ) = " ++
                show ( verifyCheckBit ( channel ( addCheckBit [[0,1,0,1,0,1,0,1], [0,0,0,0,1,1,0,0]]) ) )

    putStrLn $ "verifyCheckBit ( channel ( addCheckBit [[0,0,0,0,0,0,1,1], [1,0,1,0,1,0,1,0]]) ) = " ++
                show ( verifyCheckBit ( channel ( addCheckBit [[0,0,0,0,0,0,1,1], [1,0,1,0,1,0,1,0]] ) ) )
