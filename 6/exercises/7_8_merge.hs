merge :: Ord a => [a] -> [a] -> [a]

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | x >  y = y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve (n:ns) = splitAt (length (n:ns) `div` 2) (n:ns)

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:xs) | length (x:xs) == 1 = [x]
             | otherwise          = merge (msort first) (msort second)
                                    where half = halve (x:xs)
                                          first = fst half
                                          second = snd half


main = do
    putStrLn ""
    putStrLn $ "merge [1,2] [] = "               ++ show ( merge [1,2] [] ) 
    putStrLn $ "merge [] [1,2] = "               ++ show ( merge [] [1,2] ) 
    putStrLn $ "merge [1,3,4,6,7] [1,2,4,5] = "  ++ show ( merge [1,3,4,6,7] [1,2,4,5] )
    putStrLn $ "merge [1,2,5,9] [3,3,4,6,7] = "  ++ show ( merge [1,2,5,9] [3,3,4,6,7] )

    putStrLn ""
    putStrLn $ "msort [2] = "                    ++ show ( msort [2] ) 
    putStrLn $ "msort [6,5,10,9,3,4,1,6,7,3] = " ++ show ( msort [6,5,10,9,3,4,1,6,7,3] )
    putStrLn $ "msort [9,9,5,9,5] = "            ++ show ( msort [9,9,5,9,5] )
