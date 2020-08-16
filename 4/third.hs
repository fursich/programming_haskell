third :: [a] -> a
third = head . tail . tail


third' :: [a] -> a
third' hs = hs !! 2

third'' :: [a] -> a
third'' (x:y:z:_) = z

main = do
    print $ third [1,2,3,4,5]
    print $ third' [1,2,3,4,5]
    print $ third'' [1,2,3,4,5]
