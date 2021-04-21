module Console where
  import System.IO

  cls :: IO ()
  cls = putStr "\ESC[2J"
  
  type Pos = (Int,Int)
  
  writeat :: Pos -> String -> IO()
  writeat p xs = do
                   goto p
                   putStr xs
  
  goto :: Pos -> IO ()
  goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
  
  getCh :: IO Char
  getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

