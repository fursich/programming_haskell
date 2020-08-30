grid :: Int -> Int -> [(Int, Int)]
grid n m = [(i, j) | i <- [0..n], j <- [0..m]]

square :: Int -> [(Int, Int)]
square x = [p | p <- grid x x, p /= (0, 0) && p /= (x, x)]
