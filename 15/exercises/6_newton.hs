
prec :: Double
prec = 0.001

ini :: Double
ini = 1.0

next :: Double -> Double -> Double
next n x =  (x + n/x)/2

newton :: Double -> [Double]
newton n = iterate (next n) ini

sqroot :: Double -> Double
sqroot n = newton n !! (ct + 1)
             where ct = length(takeWhile ((> prec).abs) diffs)
                   diffs = zipWith (-) (newton n) (tail (newton n))

convergedValue :: [Double] -> Double
convergedValue (x:xs) | abs(diff) > prec = convergedValue xs
                      | otherwise        = head xs
                          where diff     = x - head xs

sqroot' :: Double -> Double
sqroot' n =  convergedValue $ newton n

main = do
    putStrLn $ "take 10 $ newton 2 = "  ++ show(take 10 $ newton 2)
    putStrLn $ "take 10 $ newton 3 = "  ++ show(take 10 $ newton 3)
    putStrLn $ "take 10 $ newton 9 = "  ++ show(take 10 $ newton 9)
    putStrLn $ "take 10 $ newton 10 = " ++ show(take 10 $ newton 10)

    putStrLn ""
    putStrLn $ "sqroot 2 = " ++ show(sqroot 2)
    putStrLn $ "sqroot 2 ^ 2 = " ++ show(sqroot 2 ^ 2)
    putStrLn $ "sqroot 3 = " ++ show(sqroot 3)
    putStrLn $ "sqroot 3 ^ 2 = " ++ show(sqroot 3 ^ 2)
    putStrLn $ "sqroot 9 = " ++ show(sqroot 9)
    putStrLn $ "sqroot 9 ^ 2 = " ++ show(sqroot 9 ^ 2)
    putStrLn $ "sqroot 10 = " ++ show(sqroot 10)
    putStrLn $ "sqroot 10 ^ 2 = " ++ show(sqroot 10 ^ 2)

    putStrLn ""
    putStrLn $ "sqroot' 2 = " ++ show(sqroot' 2)
    putStrLn $ "sqroot' 2 ^ 2 = " ++ show(sqroot' 2 ^ 2)
    putStrLn $ "sqroot' 3 = " ++ show(sqroot' 3)
    putStrLn $ "sqroot' 3 ^ 2 = " ++ show(sqroot' 3 ^ 2)
    putStrLn $ "sqroot' 9 = " ++ show(sqroot' 9)
    putStrLn $ "sqroot' 9 ^ 2 = " ++ show(sqroot' 9 ^ 2)
    putStrLn $ "sqroot' 10 = " ++ show(sqroot' 10)
    putStrLn $ "sqroot' 10 ^ 2 = " ++ show(sqroot' 10 ^ 2)


