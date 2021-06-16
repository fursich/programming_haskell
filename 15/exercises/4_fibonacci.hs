fibs :: [Integer]

fibs = gen [0,1]

gen :: [Integer] -> [Integer]
gen (x:xs) = x : gen (xs ++ next (x:xs))

next :: [Integer] -> [Integer]
next xs = [fst elm + snd elm]
            where pairs = zip xs (tail xs)
                  elm = last pairs

-- (shorter answer)

fibs' :: [Integer]
fibs' = 0 : 1: zipWith (+) fibs' (tail fibs')

-- fib = 0 : 1 : (zipWith (+) fib (tail fib))

main = do
    putStrLn $ "take 20 fibs  = " ++ show(take 20 fibs)
    putStrLn $ "take 20 fibs' = " ++ show(take 20 fibs')

