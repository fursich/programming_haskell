sum_sqrt :: Int -> Int
sum_sqrt n = sum sqrts
    where sqrts = [x^2 | x <- [1..n]]

main = print $ sum_sqrt 100
