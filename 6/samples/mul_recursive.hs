(*) :: Int -> Int -> Int
m * 0 = 0
m * n = m + ( m Main.* (n - 1) )
