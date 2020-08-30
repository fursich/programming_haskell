replicate' :: Int -> a -> [a]

replicate' n a = [a | _ <- [1..n]]
