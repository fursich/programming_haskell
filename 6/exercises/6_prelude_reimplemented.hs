-- ex.6
-- a: and :: [Bool] -> Bool

and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) | b = and'(bs)
            | not b = False

-- b: concat :: [[a]] -> [a]

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

-- c: replicate :: Int -> a -> [a]

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

-- d: (!!) :: [a] -> Int -> a

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(_:xs) !!! n | n > 0 = xs !!! (n - 1)


-- e: elem :: Eq a => a -> [a] -> Bool

elem' :: Eq a => a -> [a] -> Bool
elem' a []                 = False
elem' a (x:xs) | x == a    = True
               | otherwise = elem' a xs


main = do

    putStrLn ""
    putStrLn $ "and' [] = "                                       ++ show ( and' [] )
    putStrLn $ "and' [True] = "                                   ++ show ( and' [True] )
    putStrLn $ "and' [False] = "                                  ++ show ( and' [False] )
    putStrLn $ "and' [True, True,  True, True, True] = "          ++ show ( and' [True, True,  True, True, True] )
    putStrLn $ "and' [True, False, True, True] = "                ++ show ( and' [True, False, True, True] )

    putStrLn ""
    putStrLn $ "concat' [[1,2]] = "                               ++ show ( concat' [[1,2]] )
    putStrLn $ "concat' [[1],[2],[3]] = "                         ++ show ( concat' [[1],[2],[3]] )
    putStrLn $ "concat' [[], [1,2], [3,4,5], [], [6,7,8,9,0]] = " ++ show ( concat' [[], [1,2], [3,4,5], [], [6,7,8,9,0]] )

    putStrLn ""
    putStrLn $ "replicate' 0 1     = "                            ++ show ( replicate' 0 1 )
    putStrLn $ "replicate' 1 [1,2] = "                            ++ show ( replicate' 1 [1,2] )
    putStrLn $ "replicate' 3 \"foo\" = "                          ++ show ( replicate' 3 "foo" )
    putStrLn $ "replicate' 4 10    = "                            ++ show ( replicate' 4 10 )
    putStrLn $ "replicate' 10 'a'  = "                            ++ show ( replicate' 10 'a' )

    putStrLn ""
    putStrLn $ "[1,2] !!! 0 = "                                   ++ show ( [1,2] !!! 0 )
    putStrLn $ "[1,2] !!! 1 = "                                   ++ show ( [1,2] !!! 1 )

    putStrLn ""
    putStrLn $ "elem' 1 [] = "                                    ++ show ( elem' 1 [] )
    putStrLn $ "elem' 1 [0,1,2] = "                               ++ show ( elem' 1 [0,1,2] )
    putStrLn $ "elem' 1 [2,3,4] = "                               ++ show ( elem' 1 [2,3,4] )
    putStrLn $ "elem' [1,2] [[1],[3,3,4],[1,2]] = "               ++ show ( elem' [1,2] [[1],[3,3,4],[1,2]] )
    putStrLn $ "elem' [1,2] [[1],[3,3,4],[1,2,3]] = "             ++ show ( elem' [1,2] [[1],[3,3,4],[1,2,3]] )
