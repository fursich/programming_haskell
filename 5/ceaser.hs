import Data.Char
import Data.List

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']


let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n |  x <- ['a'..'z']]
           where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum[((o-e)^2)/e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

likelihoods :: String -> [Float]
likelihoods xs = [chisqr (rotate n fs) table | n <- [0..25]]
                 where fs = freqs xs

guess :: Int -> String -> [String]
guess n xs = [encode (-factor) xs | factor <- (take n factors)]
             where
                 factors = foldr  (++) [] [positions l lks | l <- sorted_lks]
                 sorted_lks = sort lks
                 lks = likelihoods xs

crack :: String -> String
crack xs = head (guess 1 xs)

main = do
    putStrLn $ "crack \"kdvnhoo lv ixq\" = " ++ crack "kdvnhoo lv ixq"

    putStrLn $ "crack (encode 3 \"boxing wizards jump quickly\") = " ++ crack (encode 3 "boxing wizards jump quickly")

    putStrLn $ "guess 16 (encode 3 \"boxing wizards jump quickly\") = "
    print $ guess 16 (encode 3 "boxing wizards jump quickly")
