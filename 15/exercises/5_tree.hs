
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeat' :: a -> Tree a

repeat' x = Node (repeat' x) x (repeat' x)

take' :: Int -> Tree a -> Tree a
take' 0 _ = Leaf
take' _ Leaf = Leaf
take' n (Node lhs x rhs) = Node (take' (n-1) lhs) x (take' (n-1) rhs)

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

main = do
    putStrLn $ "replicate' 3 'a' = " ++ show(replicate' 3 'a')
    putStrLn $ "replicate' 4 2 = " ++ show(replicate' 4 2)

