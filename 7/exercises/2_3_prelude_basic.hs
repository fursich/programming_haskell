
all' ::  (a  ->  Bool) -> [a] -> Bool
all' f = foldl (\x y -> x && f y) True

all'' ::  (a  ->  Bool) -> [a] -> Bool
all'' f = and . map f

any' ::  (a  ->  Bool) -> [a] -> Bool
any' f = foldl (\x y -> x || f y) False

any'' ::  (a  ->  Bool) -> [a] -> Bool
any'' f = or . map f

takeWhile' ::  (a  ->  Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x = [x] ++ takeWhile' f xs
                    | otherwise = []

dropWhile' ::  (a  ->  Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = (x:xs)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x ys -> if f x then x:ys else ys) []

main = do
    putStrLn $ "all' even [] = "                                     ++ show ( all' even [] )
    putStrLn $ "all' even [4, 2, 3] = "                              ++ show ( all' even [4, 2, 3] )
    putStrLn $ "all' even [2, 6, 8] = "                              ++ show ( all' even [2, 6, 8] )
    putStrLn $ "all' (\\x -> length x == 2) [[1, 2, 3], [4], [5, 6]] = "   ++ show ( all' (\x -> length x == 2) [[1, 2, 3], [4], [5, 6]] )
    putStrLn $ "all' (\\x -> length x == 2) [[1, 2], [3, 4], [5, 6]] = "   ++ show ( all' (\x -> length x == 2) [[1, 2], [3, 4], [5, 6]] )

    putStrLn $ "all'' even [] = "                                     ++ show ( all'' even [] )
    putStrLn $ "all'' even [4, 2, 3] = "                              ++ show ( all'' even [4, 2, 3] )
    putStrLn $ "all'' even [2, 6, 8] = "                              ++ show ( all'' even [2, 6, 8] )
    putStrLn $ "all'' (\\x -> length x == 2) [[1, 2, 3], [4], [5, 6]] = "   ++ show ( all'' (\x -> length x == 2) [[1, 2, 3], [4], [5, 6]] )
    putStrLn $ "all'' (\\x -> length x == 2) [[1, 2], [3, 4], [5, 6]] = "   ++ show ( all'' (\x -> length x == 2) [[1, 2], [3, 4], [5, 6]] )

    putStrLn $ "any' even [] = "                                     ++ show ( any' even [] )
    putStrLn $ "any' even [1, 2, 3] = "                              ++ show ( any' even [1, 2, 3] )
    putStrLn $ "any' odd [2, 6, 8] = "                               ++ show ( any' odd [2, 6, 8] )
    putStrLn $ "any' (\\x -> length x /= 2) [[1, 2, 3], [4], [5, 6]] = "   ++ show ( any' (\x -> length x /= 2) [[1, 2, 3], [4], [5, 6]] )
    putStrLn $ "any' (\\x -> length x /= 2) [[1, 2], [3, 4], [5, 6]] = "   ++ show ( any' (\x -> length x /= 2) [[1, 2], [3, 4], [5, 6]] )

    putStrLn $ "any'' even [] = "                                     ++ show ( any'' even [] )
    putStrLn $ "any'' even [1, 2, 3] = "                              ++ show ( any'' even [1, 2, 3] )
    putStrLn $ "any'' odd [2, 6, 8] = "                               ++ show ( any'' odd [2, 6, 8] )
    putStrLn $ "any'' (\\x -> length x /= 2) [[1, 2, 3], [4], [5, 6]] = "   ++ show ( any'' (\x -> length x /= 2) [[1, 2, 3], [4], [5, 6]] )
    putStrLn $ "any'' (\\x -> length x /= 2) [[1, 2], [3, 4], [5, 6]] = "   ++ show ( any'' (\x -> length x /= 2) [[1, 2], [3, 4], [5, 6]] )

    putStrLn $ "takeWhile' even [] = "                                     ++ show ( takeWhile' even [] )
    putStrLn $ "takeWhile' even [4, 2, 3] = "                              ++ show ( takeWhile' even [4, 2, 3] )
    putStrLn $ "takeWhile' even [2, 3, 8] = "                              ++ show ( takeWhile' even [2, 3, 8] )
    putStrLn $ "takeWhile' (\\x -> length x == 2) [[1, 2, 3], [4], [5, 6]] = "   ++ show ( takeWhile' (\x -> length x == 2) [[1, 2, 3], [4], [5, 6]] )
    putStrLn $ "takeWhile' (\\x -> length x == 2) [[1, 2], [3, 4], [5, 6]] = "   ++ show ( takeWhile' (\x -> length x == 2) [[1, 2], [3, 4], [5, 6]] )

    putStrLn $ "dropWhile' even [] = "                                     ++ show ( dropWhile' even [] )
    putStrLn $ "dropWhile' even [4, 2, 3] = "                              ++ show ( dropWhile' even [4, 2, 3] )
    putStrLn $ "dropWhile' even [2, 3, 8] = "                              ++ show ( dropWhile' even [2, 3, 8] )
    putStrLn $ "dropWhile' (\\x -> length x == 2) [[1, 2, 3], [4], [5, 6]] = "   ++ show ( dropWhile' (\x -> length x == 2) [[1, 2, 3], [4], [5, 6]] )
    putStrLn $ "dropWhile' (\\x -> length x == 2) [[1, 2], [3, 4], [5, 6]] = "   ++ show ( dropWhile' (\x -> length x == 2) [[1, 2], [3, 4], [5, 6]] )

    putStrLn $ "map' (\\x -> x * 2) [1, 2, 3] = "                    ++ show ( map' (\x -> x * 2) [1, 2, 3] )
    putStrLn $ "map' reverse [[1, 2, 3], [4, 5, 6], [7, 8]] = "      ++ show ( map' reverse [[1, 2, 3], [4, 5, 6], [7, 8]] )

    putStrLn $ "filter' even [1, 2, 3] = "                           ++ show ( filter' even [1, 2, 3] )
    putStrLn $ "filter' (\\x -> length x == 2) [[1, 2, 3], [4], [5, 6]] = "   ++ show ( filter' (\x -> length x == 2) [[1, 2, 3], [4], [5, 6]] )
