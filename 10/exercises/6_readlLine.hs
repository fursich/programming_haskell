import System.IO

readOne :: String -> IO String
readOne xs0 = do
  x <- getCh
  if x == '\b' then
    do
      if length xs0 > 0 then
        do
          putChar '\b'
          putChar '\DEL'
          putChar '\b'
          xs <- readOne (init xs0)
          return xs
      else
        do
          xs <- readOne ""
          return xs
  else
    do
      putChar x
      if x == '\n' then
        return xs0
      else
        do
          xs <- readOne (xs0++[x])
          return xs

readline :: IO String 
readline = readOne ""
