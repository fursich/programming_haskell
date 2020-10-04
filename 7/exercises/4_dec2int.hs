dec2int :: [Int] -> Int

dec2int = foldl (\x y -> 10 * x + y)  0

main = do
    putStrLn $ "dec2int [] = "                       ++ show ( dec2int [] )
    putStrLn $ "dec2int [0, 0, 1] = "                ++ show ( dec2int [0, 0, 1] )
    putStrLn $ "dec2int [5, 2, 3, 7] = "             ++ show ( dec2int [5, 2, 3, 7] )
    putStrLn $ "dec2int [3, 4, 5, 9, 0, 3, 0, 1] = " ++ show ( dec2int [3, 4, 5, 9, 0, 3, 0, 1] )
