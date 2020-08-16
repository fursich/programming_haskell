mult' :: Int -> Int -> Int -> Int
mult' = \x -> (\y -> (\z -> x * y * z)) 

main = do
    print $ mult' 2 3 4
