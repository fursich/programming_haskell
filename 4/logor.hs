logor :: Bool -> Bool  -> Bool
True  `logor` True  = True
True  `logor` False = True
False `logor` True  = True
False `logor` False = False

logor' :: Bool -> Bool  -> Bool
False `logor'` False = False
_ `logor'` _ = True

logor'' :: Bool -> Bool  -> Bool
True `logor''` _ = True
False `logor''` b = b

logor''' :: Bool -> Bool -> Bool
b `logor'''` c | b == c = b
               | otherwise = True

printAllCombinations f = do
    putStrLn "\nvalidating.."
    print $ True  `f` True
    print $ True  `f` False
    print $ False `f` True
    print $ False `f` False
    printIsValidLogor f

printIsValidLogor f | (True `f` True == True) && (True `f` False == True) && (False `f` True == True) && (False `f` False == False)
                        = putStrLn " => OK"
                    | otherwise = putStrLn " => NG"

main = do
    printAllCombinations logor
    printAllCombinations logor'
    printAllCombinations logor''
    printAllCombinations logor'''
