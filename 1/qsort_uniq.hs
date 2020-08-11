qsort'' :: Ord n => [n] -> [n]
qsort'' [] = []
qsort'' (x:xs) = qsort'' smaller ++ [x] ++ qsort'' larger
                where
                    larger  = [n| n <- xs, n > x]
                    smaller = [n| n <- xs, n < x]

main = print(qsort''([2,2,3,1,1]))
