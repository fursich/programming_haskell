even' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

main = do
    putStrLn $ "even 100 ="
    print $ even 100

    putStrLn $ "evens [0,1,2,3,4,5,6,7,8] ="
    print $ evens [0,1,2,3,4,5,6,7,8]

