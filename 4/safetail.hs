safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' as = tail as

main = do
    print $ safetail ([] :: [Int])
    print $ safetail [1,2,3]

    print $ safetail' ([] :: [Int])
    print $ safetail' [1,2,3]

    print $ safetail'' ([] :: [Int])
    print $ safetail'' [1,2,3]
