
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v []     = v
foldl' f v (x:xs) = foldl' f (f v x) xs

sum'     = foldl' (+) 0
product' = foldl' (*) 1
or'      = foldl' (||) False
and'     = foldl' (&&) True

length'  = foldl' (\x _ -> 1 + x) 0
reverse' = foldl' (\xs y -> [y] ++ xs) []

main = do
    putStrLn $ "sum' [] = "                          ++ show ( sum' [] )
    putStrLn $ "sum' [1, 2, 3, 4, 5] = "             ++ show ( sum' [1, 2, 3, 4, 5] )
    putStrLn $ "sum' [2, 10] = "                     ++ show ( sum' [2, 10] )

    putStrLn $ "product' [] = "                      ++ show ( product' [] )
    putStrLn $ "product' [1, 2, 3, 4, 5] = "         ++ show ( product' [1, 2, 3, 4, 5] )
    putStrLn $ "product' [2, 10] = "                 ++ show ( product' [2, 10] )

    putStrLn $ "or' [] = "                           ++ show ( or' [] )
    putStrLn $ "or' [False, False, True, False] = "  ++ show ( or' [False, False, True, False] )
    putStrLn $ "or' [False, False, False, False] = " ++ show ( or' [False, False, False, False] )

    putStrLn $ "and' [] = "                          ++ show ( and' [] )
    putStrLn $ "and' [True, True, False, True] = "   ++ show ( and' [True, True, False, True] )
    putStrLn $ "and' [True, True, True, True] = "    ++ show ( and' [True, True, True, True] )

    putStrLn $ "length' [] = "                       ++ show ( length' [] )
    putStrLn $ "length' [1, 2, 3, 4, 5] = "          ++ show ( length' [1, 2, 3, 4, 5] )
    putStrLn $ "length' [2, 10] = "                  ++ show ( length' [2, 10] )

    putStrLn $ "reverse' [10] = "                     ++ show ( reverse' [10] )
    putStrLn $ "reverse' [1, 2, 3, 4, 5] = "          ++ show ( reverse' [1, 2, 3, 4, 5] )
    putStrLn $ "reverse' [2, 10] = "                  ++ show ( reverse' [2, 10] )
