x = [(a,b) | a <- [1, 2, 3], b <- [4, 5, 6]]

y = concat [[(a, b) | b <- [4, 5, 6]] | a <- [1, 2, 3]]