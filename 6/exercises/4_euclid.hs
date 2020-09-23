euclid :: Int -> Int -> Int
euclid x y | x < 0 = euclid (-x) y
           | y < 0 = euclid x (-y)
           | x == 0 || y == 0 = 0
           | x == y = x
           | x > y  = euclid (x - y) y
           | y > x  = euclid x (y - x)

main = do
    putStrLn $ "euclid 1 1 = "      ++ show ( euclid     1     1  )
    putStrLn $ "euclid 12 1 = "     ++ show ( euclid    12     1  )
    putStrLn $ "euclid 1 10 = "     ++ show ( euclid     1    10  )
    putStrLn $ "euclid 60 48 = "    ++ show ( euclid    60    48  )
    putStrLn $ "euclid 1 0 = "      ++ show ( euclid     1     0  )
    putStrLn $ "euclid 0 0 = "      ++ show ( euclid     0     0  )
    putStrLn $ "euclid -126 -54 = " ++ show ( euclid (-126) (-54) )
    putStrLn $ "euclid 126 -54 = "  ++ show ( euclid   126  (-54) )
    putStrLn $ "euclid -126 54 = "  ++ show ( euclid (-126)   54  )
