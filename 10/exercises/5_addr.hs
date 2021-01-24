import Data.Char

getDigit :: String -> IO Int
getDigit prompt = do
                    putStr prompt
                    x <- getChar
                    newline
                    if isDigit x then
                      return (digitToInt x)
                    else
                      do
                        putStrLn "ERROR: Invalid digit"
                        getDigit prompt

newline :: IO ()
newline = putChar '\n'

adder :: IO ()
adder = do
  newline
  cnt <- getDigit "How many numbers? "
  nums <- sequence (replicate cnt (getDigit ""))

  putStr "The total is : "
  putStr (show $ sum nums)
  newline

