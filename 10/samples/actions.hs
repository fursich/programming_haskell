strlen :: IO ()
strlen = do
  putStr "Enter a string: "
  xs <- getLine
  putStr "The string has "
  putStr (show (length xs))
  putStrLn " characters"
