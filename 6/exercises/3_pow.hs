(^) :: Int -> Int -> Int
x ^ 0 = 1
x ^ n | n > 0 = x * ( x Main.^ (n - 1) )
