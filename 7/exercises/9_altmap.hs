

indexList :: (Integral i, Enum i) => [a] -> [(i, a)]
indexList xs = zip [0..] xs

altApply :: Integral i => (a -> b) -> (a -> b) -> (i,a) -> b
altApply f g (i,a) | even i = f a
                   | otherwise = g a

altMapByIndex :: Integral i => (a -> b) -> (a -> b) -> [(i,a)] -> [b]
altMapByIndex f g = map (altApply f g)

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = altMapByIndex f g . indexList

luhnDouble :: Int -> Int
luhnDouble x | x < 5     = 2 * x
             | otherwise = 2 * x - 9

luhn :: [Int] -> Bool
luhn xs = sum (altMap id luhnDouble (reverse xs)) `mod` 10 == 0

main = do
    putStrLn $ "indexList [1,2,3] = "             ++ show ( indexList [1,2,3] )
    putStrLn $ "indexList ['a', 'b', 'c'] = "     ++ show ( indexList ['a', 'b', 'c'] )

    putStrLn $ "altApply (+10) (*100) (0,2) = "   ++ show ( altApply (+10) (*100) (0,2) )
    putStrLn $ "altApply (+10) (*100) (5,1) = "   ++ show ( altApply (+10) (*100) (5,1) )

    putStrLn $ "altMapByIndex (+10) (*100) [(5,1), (4,1), (0,1), (1,3)] = " ++ show ( altMapByIndex (+10) (*100) [(5,1), (4,1), (0,1), (1,3)] )
    putStrLn $ "altMapByIndex (+10) (*100) [(2,1), (3,1), (4,1), (5,3)] = " ++ show ( altMapByIndex (+10) (*100) [(2,1), (3,1), (4,1), (5,3)] )

    putStrLn $ "altMap (+10) (*100) [1,2,3,4,5] = " ++ show ( altMap (+10) (*100) [1,2,3,4,5] )
    putStrLn $ "altMap (reverse) (concat.(replicate 3)) [\"foo\", \"bar\", \"baz\", \"foobar\", \"boo\"] = "
               ++ show ( altMap (reverse) (concat.(replicate 3)) ["foo", "bar", "baz", "foobar", "boo"]  )

    putStrLn $ "luhn [1,7,8,4] = "                           ++ show ( luhn [1,7,8,4] )
    putStrLn $ "luhn [1,7,8,3] = "                           ++ show ( luhn [1,7,8,3] )
    putStrLn $ "luhn [1,7,4,8] = "                           ++ show ( luhn [1,7,4,8] )
    -- https://pay.jp/docs/testcard
    putStrLn $ "luhn [4,0,1,2,8,8,8,8,8,8,8,8,1,8,8,1] = "   ++ show ( luhn [4,0,1,2,8,8,8,8,8,8,8,8,1,8,8,1] )
    -- https://pay.jp/docs/testcard
    putStrLn $ "luhn [3,7,8,2,8,2,2,4,6,3,1,0,0,0,5] = "     ++ show ( luhn [3,7,8,2,8,2,2,4,6,3,1,0,0,0,5] )
