compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

reverseSqrEven = compose [reverse, map (^2), filter even]

main = do
    putStrLn $ "reverseSqrEven [] = "                          ++ show ( reverseSqrEven [] )
    putStrLn $ "reverseSqrEven [1, 2, 3, 4, 5] = "             ++ show ( reverseSqrEven [1, 2, 3, 4, 5] )
    putStrLn $ "reverseSqrEven [2, 10] = "                     ++ show ( reverseSqrEven [2, 10] )
