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

nextNumber :: Int -> Int -> IO Int
nextNumber ttl cnt = do
  if cnt <= 0 then
    return ttl
  else
    do
      num <- getDigit ""
      x <- nextNumber (num+ttl) (cnt-1)
      return x

adder :: IO ()
adder = do
  newline
  cnt <- getDigit "How many numbers? "
  ttl <- nextNumber 0 cnt

  putStr "The total is : "
  putStr (show ttl)
  newline

