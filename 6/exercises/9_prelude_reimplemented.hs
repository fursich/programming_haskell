-- ex.9
-- a: sum
sum'' :: [Int] -> Int
sum'' [] = 0
sum'' (n:ns) = n + sum(ns)

-- b: take
take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : (take' (n - 1) xs)

-- c: last
last' :: [a] -> a
last' (x:xs) | null xs   = x
             | otherwise = last' xs

main = do

    putStrLn ""
    putStrLn $ "sum'' [] = "            ++ show ( sum'' [] )
    putStrLn $ "sum'' [1] = "           ++ show ( sum'' [1] )
    putStrLn $ "sum'' [1,2,3,4,5] = "   ++ show ( sum'' [1,2,3,4,5] )

    putStrLn ""
    putStrLn $ "take' 0 [] = "          ++ show ( take' 0 []::[Int] )
    putStrLn $ "take' 1 [] = "          ++ show ( take' 1 []::[Int] )
    putStrLn $ "take' 7 [1] = "         ++ show ( take' 7 [1] )
    putStrLn $ "take' 1 [1] = "         ++ show ( take' 1 [1] )
    putStrLn $ "take' 3 [1,2,3,4,5] = " ++ show ( take' 3 [1,2,3,4,5] )

    putStrLn ""
    putStrLn $ "last' [1] = "           ++ show ( last' [1] )
    putStrLn $ "last' [1,2,3,4,5] = "   ++ show ( last' [1,2,3,4,5] )
