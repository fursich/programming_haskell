init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs
