
import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bins = sum [w * b | (w, b) <- zip weights bins]
               where weights = iterate (*2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 =  []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits  ++ repeat 0)

encode ::String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits  = take  8 bits : chop8 (drop 8 bits)

decode :: [Bit]  ->  String
decode = map ( chr . bin2int ) . chop8

transmit :: String -> String
transmit = decode .channel . encode

channel  ::  [Bit] -> [Bit]
channel = id

main = do
    putStrLn $ "bin2int [] = "                         ++ show ( bin2int [] )
    putStrLn $ "bin2int [0, 1] = "                     ++ show ( bin2int [0, 1] )
    putStrLn $ "bin2int [1, 1, 1, 1] = "               ++ show ( bin2int [1, 1, 1, 1] )

    putStrLn $ "bin2int' [] = "                        ++ show ( bin2int' [] )
    putStrLn $ "bin2int' [0, 1] = "                    ++ show ( bin2int' [0, 1] )
    putStrLn $ "bin2int' [1, 1, 1, 1] = "              ++ show ( bin2int' [1, 1, 1, 1] )

    putStrLn $ "int2bin 0 = "                          ++ show ( int2bin 0 )
    putStrLn $ "int2bin 2 = "                          ++ show ( int2bin 2 )
    putStrLn $ "int2bin 15 = "                         ++ show ( int2bin 15 )

    putStrLn $ "make8 [] = "                           ++ show ( make8 [] )
    putStrLn $ "make8 [0, 1] = "                       ++ show ( make8 [0, 1] )
    putStrLn $ "make8 [1, 1, 1, 1] = "                 ++ show ( make8 [1, 1, 1, 1] )

    putStrLn $ "chop8 [] = "                           ++ show ( chop8 [] )
    putStrLn $ "chop8 [0,1,0,1,0,1,0,1,0,1,0,1,0] = "  ++ show ( chop8 [0,1,0,1,0,1,0,1,0,1,0,1,0] )

    putStrLn $ "encode \"abc\" = "                     ++ show ( encode "abc" )
    putStrLn $ "decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] = " ++ show ( decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] )

    putStrLn $ "transmit \"higher order function\" = " ++ show ( transmit "higher order function" )
