scalarproduct :: [Int] -> [Int] -> Int
scalarproduct ns ms = sum [n * m | (n, m) <- zip ns ms]
