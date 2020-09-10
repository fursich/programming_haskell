product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product ns

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse'(x:xs) = reverse' xs ++ [x]

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x: (xs +++ ys)

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

main = do
    putStrLn $ "product' [4,1,2,10] = "
    print $ product' [4,1,2,10]

    putStrLn $ "length' [1,4,8,2,10] = "
    print $ length' [1,4,8,2,10]

    putStrLn $ "reverse' [5,1,4,8,0]= "
    print $ reverse' [5,1,4,8,0]

    putStrLn $ "[2,5,1] +++ [4,8,0]= "
    print $ [2,5,1] +++ [4,8,0]

    putStrLn $ "insert 5 [3,4,8] ="
    print $ insert 5 [3,4,8]

    putStrLn $ "isort [8,3,1,3,10,7,5] ="
    print $ isort [8,3,1,3,10,7,5]

