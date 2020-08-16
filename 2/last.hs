last' = head . reverse
last'' a = a !! (length a - 1)

main = do
    print(last' [1,2,3,4,5])
    print(last'' [1,2,3,4,5])
