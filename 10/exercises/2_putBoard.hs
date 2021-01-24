import Data.Char

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Int -> Board
initial 0 = []
initial x = x: (initial (x-1))

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = 1 <= num && num <= cnt where cnt = board !! (row-1)

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1..] board]
                     where update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
                   putStr (show row)
                   putStr ": "
                   putStrLn (concat (replicate num "*"))

putBoard :: Board -> IO ()
putBoard xs = sequence_ [putRow r x | (r, x) <- zip [1..] xs ]

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

play :: Board -> Int -> IO ()
play board player =
  do
    newline
    putBoard board
    if finished board then
      do
        newline
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
    else
      do
        newline
        putStr "Player "
        putStrLn (show player)
        row <- getDigit "Enter a row number: "
        num <- getDigit "Stars to remove : "
        if valid board row num then
          play (move board row num) (next player)
        else
          do
            newline
            putStrLn "ERROR: Invalid move"
            play board player

nim :: IO ()
nim = do
  newline
  rows <- getDigit "How many rows?: "
  if rows > 0 then
    play (initial rows) 1
  else
    do
      newline
      putStrLn "ERROR: Invalid number of rows"
      nim

