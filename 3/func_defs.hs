bools :: [Bool]
bools = [False, True, False]

nums :: [[Int]]
nums = [[1,2,3,4,5], [1,2,3], [123,1000,-100]]

add :: Int -> Int -> Int ->  Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f a = f a

main = do
    print $ "bools : " ++ show bools 
    print $ "nums : " ++ show nums
    print $ "add 1 2 3 : " ++ show(add 1 2 3)
    print $ "copy [123, 456] : " ++ show(copy [123, 456])
    print $ "apply add 1 2 3: " ++ show(apply add 1 2 3)
