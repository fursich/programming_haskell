logand :: Bool -> Bool -> Bool
logand b c = if b then if c then True else False else False

logand' :: Bool -> Bool -> Bool
logand' b c = if b then c else False

printAllCombinations f = do
    putStrLn "\nvalidating.."
    print $ True  `f` True
    print $ True  `f` False
    print $ False `f` True
    print $ False `f` False
    printIsValidLogand f

printIsValidLogand f | (True `f` True == True) && (True `f` False == False) && (False `f` True == False) && (False `f` False == False)
                        = putStrLn " => OK"
                    | otherwise = putStrLn " => NG"

main = do
    printAllCombinations logand
    printAllCombinations logand'
