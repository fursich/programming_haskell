halve xs | even $ length xs = splitAt (length xs `div` 2) xs

main = print $ halve [1,2,3,4,5,6]
