putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs ]

-- another solution
putStr'' :: String -> IO ()
putStr'' x = sequence_ (putChars x)

putChars [] = []
putChars (x:xs) = putChar x : putChars xs

