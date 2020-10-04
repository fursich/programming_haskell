lc1 :: (a -> a) -> (a -> Bool) -> [a] -> [a]
lc1 f p xs = [f x | x <- xs, p x]

lc2 :: (a -> a) -> (a -> Bool) -> [a] -> [a]
lc2 f p = map f . filter p

double a = 2 * a

main = do
    putStrLn $ "lc1 double even [] = " ++ show ( lc1 double even [] )
    putStrLn $ "lc2 double even [1,2,3,4,5,6] = " ++ show ( lc1 double even [1,2,3,4,5,6] )

    putStrLn $ "lc1 double even [] = " ++ show ( lc1 double even [] )
    putStrLn $ "lc2 double even [1,2,3,4,5,6] = " ++ show ( lc1 double even [1,2,3,4,5,6] )
