
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x y -> f(x, y)

uncurry' :: (a -> b -> c) -> (a , b) -> c
uncurry' f (x, y) = f x y

sum'  :: (Int, Int) -> Int
sum' (x, y) = x + y

sum'' :: Int -> Int -> Int
sum'' x y = x + y

main = do
    putStrLn $ "(curry' sum' 1) 2 = "  ++ show ( (curry' sum' 1) 2 )
    putStrLn $ "(curry' sum' 3) 6 = "  ++ show ( (curry' sum' 3) 6 )

    putStrLn $ "(uncurry' sum'' (1, 2) = "  ++ show ( uncurry' sum'' (1, 2) )
    putStrLn $ "(uncurry' sum'' (3, 6) = "  ++ show ( uncurry' sum'' (3, 6) )
