qsort' :: Ord n => [n] -> [n]
qsort' [] = []
qsort' (x:xs) = qsort' larger ++ [x] ++ qsort' smaller
                where
                    larger  = [n| n <- xs, n >= x]
                    smaller = [n| n <- xs, n < x]

main = print(qsort'([2,3,4,5,1,6]))
