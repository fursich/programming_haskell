luhnDouble :: Int -> Int
luhnDouble x | 0 < x && x <= 4 = 2 * x
             | x <= 9 = 2 * x - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = (luhnDouble(x) + y + luhnDouble(z) + w) `mod` 10 == 0

main = do
    print $ luhnDouble 1
    print $ luhnDouble 2
    print $ luhnDouble 4
    print $ luhnDouble 5
    print $ luhnDouble 7
    print $ luhnDouble 8
    print $ luhnDouble 9
    print $ luhn 1 7 8 4
    print $ luhn 4 7 8 3
