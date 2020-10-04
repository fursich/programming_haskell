
unfold' :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold' p h t x | p x = []
                | otherwise = h x : unfold' p h t (t x)

mod8 x = x `mod` 8 == 0
pow3 x = a * a * a
        where a = fromIntegral x :: Double
plus1 x = x + 1

type Bit = Int
int2bin :: Int -> [Bit]
int2bin = unfold' (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold' null (take 8) (drop 8)

map' :: (a -> b) -> [a]  ->  [b]
map' f =  unfold' null (f.head) tail

iterate' :: (a -> a)  -> a -> [a]
iterate' f = unfold' (\_ -> False) id f

main = do
    putStrLn $ "unfold' mod8 pow3 plus1 = " ++ show ( unfold' mod8 pow3 plus1 1)

    putStrLn $ "int2bin 0 = "                          ++ show ( int2bin 0 )
    putStrLn $ "int2bin 2 = "                          ++ show ( int2bin 2 )
    putStrLn $ "int2bin 15 = "                         ++ show ( int2bin 15 )

    putStrLn $ "chop8 [] = "                           ++ show ( chop8 [] )
    putStrLn $ "chop8 [1,2] = "                        ++ show ( chop8 [1,2] )
    putStrLn $ "chop8 [0,1,0,1,0,1,0,1,0,1,0,1,0] = "  ++ show ( chop8 [0,1,0,1,0,1,0,1,0,1,0,1,0] )

    putStrLn $ "map' (\\x -> x * 2) [1, 2, 3] = "                    ++ show ( map' (\x -> x * 2) [1, 2, 3] )
    putStrLn $ "map' reverse [[1, 2, 3], [4, 5, 6], [7, 8]] = "      ++ show ( map' reverse [[1, 2, 3], [4, 5, 6], [7, 8]] )

    putStrLn $ "take 5 $ iterate' (\\x ->  x * 3) 1 = " ++ show ( take 5 $ iterate' (\x -> x * 3) 1 )
